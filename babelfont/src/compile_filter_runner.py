import importlib
import json
import sys
import zipfile
from pathlib import Path

GROUP = "counterpunch_compilefilter_plugins"
LAYER_KEY = "\x1f"


def _entry_points(wheels_dir):
    found = []
    for wheel in sorted(Path(wheels_dir).glob("*.whl")):
        sys.path.insert(0, str(wheel))
        with zipfile.ZipFile(wheel) as archive:
            names = [
                name
                for name in archive.namelist()
                if name.endswith(".dist-info/entry_points.txt")
            ]
            if not names:
                continue
            text = archive.read(names[0]).decode("utf-8")
        section = None
        for line in text.splitlines():
            stripped = line.strip()
            if stripped.startswith("[") and stripped.endswith("]"):
                section = stripped[1:-1]
                continue
            if section != GROUP or "=" not in stripped or stripped.startswith("#"):
                continue
            name, target = [part.strip() for part in stripped.split("=", 1)]
            module_name, _, attr = target.partition(":")
            found.append((name, module_name, attr))
    return found


def _load(wheels_dir, ids):
    wanted = set(ids)
    plugins = []
    for name, module_name, attr in _entry_points(wheels_dir):
        if name not in wanted:
            continue
        module = importlib.import_module(module_name)
        plugin = getattr(module, attr)()
        plugins.append(plugin)
    order = {plugin_id: index for index, plugin_id in enumerate(ids)}
    plugins.sort(key=lambda plugin: order.get(getattr(plugin, "id", ""), 1_000_000))
    return plugins


def _master_id(layer):
    master = layer.get("master")
    if isinstance(master, dict):
        return master.get("master") or master.get("content")
    return None


def _ctx(font, layer):
    master_id = _master_id(layer)
    masters = font.get("masters") or []
    master = next((item for item in masters if item.get("id") == master_id), None)
    settings = (
        ((font.get("format_specific") or {}).get("com.counterpunch.plugin-settings") or {})
    )
    plugin_settings = {}
    return {
        "master": master,
        "masterId": master_id,
        "axes": font.get("axes") or [],
        "settings": plugin_settings,
        "allSettings": settings,
    }


def _ctx_for(plugin, font, layer):
    ctx = _ctx(font, layer)
    values = (ctx["allSettings"].get(getattr(plugin, "id", ""), {}) or {}).get("values") or {}
    ctx["settings"] = values
    return ctx


def _layer_key(glyph_name, layer):
    return f"{glyph_name}{LAYER_KEY}{layer.get('id') or ''}"


def _components(layer):
    names = []
    for shape in layer.get("shapes") or []:
        if isinstance(shape, dict) and shape.get("reference"):
            names.append(shape["reference"])
    return names


def compile_font(plugins, font):
    glyphs = font.get("glyphs") or []
    by_name = {glyph.get("name"): glyph for glyph in glyphs if glyph.get("name")}
    pending = []
    for glyph in glyphs:
        if glyph.get("exported") is False:
            continue
        for layer in glyph.get("layers") or []:
            if layer.get("is_background"):
                continue
            pending.append((glyph["name"], layer))

    def depth(glyph_name, seen):
        if glyph_name in seen:
            return 0
        glyph = by_name.get(glyph_name)
        if not glyph:
            return 0
        seen = seen | {glyph_name}
        best = 0
        for layer in glyph.get("layers") or []:
            for reference in _components(layer):
                best = max(best, 1 + depth(reference, seen))
        return best

    pending.sort(key=lambda item: depth(item[0], frozenset()))
    replaced = {}
    current = {}
    for glyph_name, layer in pending:
        key = _layer_key(glyph_name, layer)
        working = json.loads(json.dumps(layer))
        for plugin in plugins:
            ctx = _ctx_for(plugin, font, working)
            layer_fn = getattr(plugin, "filter_layer_json", None)
            if callable(layer_fn):
                updated = layer_fn(working, ctx)
                if isinstance(updated, dict):
                    working = updated
            propagated = getattr(plugin, "filter_layer_json_propagated_anchors", None)
            if callable(propagated):
                updated = propagated(working, ctx)
                if isinstance(updated, dict):
                    working = updated
        if working != layer:
            replaced[key] = working
        current[key] = working

    compile_font_json = json.loads(json.dumps(font))
    for glyph in compile_font_json.get("glyphs") or []:
        name = glyph.get("name")
        layers = []
        for layer in glyph.get("layers") or []:
            layers.append(current.get(_layer_key(name, layer), layer))
        glyph["layers"] = layers

    feature_chunks = []
    for plugin in plugins:
        font_fn = getattr(plugin, "filter_font_json", None)
        if not callable(font_fn):
            continue
        ctx = _ctx_for(plugin, compile_font_json, {})
        result = font_fn(compile_font_json, ctx)
        if not isinstance(result, dict):
            continue
        extra_layers = result.get("layers") or {}
        if isinstance(extra_layers, dict):
            replaced.update(extra_layers)
        features = result.get("features")
        if isinstance(features, str) and features.strip():
            feature_chunks.append(features.strip())
    return {"layers": replaced, "features": "\n".join(feature_chunks)}


def main():
    wheels_dir = sys.argv[1]
    payload = json.load(sys.stdin)
    plugins = _load(wheels_dir, payload.get("ids") or [])
    op = payload.get("op")
    if op == "probe":
        propagated = any(
            callable(getattr(plugin, "filter_layer_json_propagated_anchors", None))
            for plugin in plugins
        )
        json.dump({"propagated": propagated}, sys.stdout)
        return
    if op == "compile":
        json.dump(compile_font(plugins, payload.get("font") or {}), sys.stdout)
        return
    json.dump({"error": f"unknown op {op}"}, sys.stdout)


if __name__ == "__main__":
    main()
