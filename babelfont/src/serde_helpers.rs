use std::{collections::HashMap, str::FromStr};

use fontdrasil::{
    coords::{DesignCoord, DesignLocation, UserCoord},
    types::Tag,
};
use indexmap::IndexMap;
use serde::{
    ser::{SerializeMap as _, SerializeSeq as _},
    Deserialize as _,
};
use smol_str::SmolStr;

pub(crate) fn kerning_map<S>(
    map: &IndexMap<(SmolStr, SmolStr), i16>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let mut ser_map = serializer.serialize_map(Some(map.len()))?;
    for ((left, right), value) in map {
        let key = format!("{}:{}", left, right);
        ser_map.serialize_entry(&key, value)?;
    }
    ser_map.end()
}

pub(crate) fn kerning_unmap<'de, D>(
    deserializer: D,
) -> Result<IndexMap<(SmolStr, SmolStr), i16>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let raw_map: HashMap<String, i16> = HashMap::deserialize(deserializer)?;
    let mut map = IndexMap::new();
    for (key, value) in raw_map {
        let parts: Vec<&str> = key.splitn(2, ':').collect();
        if parts.len() != 2 {
            return Err(serde::de::Error::custom(format!(
                "Invalid kerning key format: {}",
                key
            )));
        }
        map.insert((SmolStr::from(parts[0]), SmolStr::from(parts[1])), value);
    }
    Ok(map)
}

pub(crate) fn usercoord_option_ser<S>(
    value: &Option<UserCoord>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    match value {
        Some(v) => serializer.serialize_f64(v.to_f64()),
        None => serializer.serialize_none(),
    }
}

pub(crate) fn usercoord_option_de<'de, D>(deserializer: D) -> Result<Option<UserCoord>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let opt: Option<f64> = Option::deserialize(deserializer)?;
    Ok(opt.map(UserCoord::new))
}

pub(crate) fn axismap_ser<S>(
    map: &Option<Vec<(UserCoord, DesignCoord)>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    match map {
        Some(pairs) => {
            let mut ser_vec = serializer.serialize_seq(Some(pairs.len()))?;
            for (user, design) in pairs {
                ser_vec.serialize_element(&(user.to_f64(), design.to_f64()))?;
            }
            ser_vec.end()
        }
        None => serializer.serialize_none(),
    }
}

pub(crate) fn axismap_de<'de, D>(
    deserializer: D,
) -> Result<Option<Vec<(UserCoord, DesignCoord)>>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let opt: Option<Vec<(f64, f64)>> = Option::deserialize(deserializer)?;
    Ok(opt.map(|pairs| {
        pairs
            .into_iter()
            .map(|(u, d)| (UserCoord::new(u), DesignCoord::new(d)))
            .collect()
    }))
}

pub(crate) fn decomposed_is_identity(
    decomposed: &crate::common::decomposition::DecomposedAffine,
) -> bool {
    decomposed.translation == (0.0, 0.0)
        && decomposed.scale == (1.0, 1.0)
        && decomposed.rotation == 0.0
        && decomposed.skew == (0.0, 0.0)
}

pub(crate) fn is_one_one(scale: &(f64, f64)) -> bool {
    *scale == (1.0, 1.0)
}

pub(crate) fn one_one() -> (f64, f64) {
    (1.0, 1.0)
}

pub(crate) fn is_zero<T>(f: &T) -> bool
where
    T: PartialEq + From<f32>,
{
    f == &T::from(0.0)
}

pub(crate) fn design_location_to_map<S>(
    location: &DesignLocation,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let mut ser_map = serializer.serialize_map(Some(location.iter().count()))?;
    for (axis, coord) in location.iter() {
        ser_map.serialize_entry(axis, &coord.to_f64())?;
    }
    ser_map.end()
}

pub(crate) fn design_location_from_map<'de, D>(deserializer: D) -> Result<DesignLocation, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let raw_map: HashMap<String, f64> = HashMap::deserialize(deserializer)?;
    let mut location = DesignLocation::default();
    for (axis, value) in raw_map {
        location.insert(
            Tag::from_str(&axis).map_err(serde::de::Error::custom)?,
            DesignCoord::new(value),
        );
    }
    Ok(location)
}

pub(crate) fn string_design_location_to_map<S>(
    location: &IndexMap<String, DesignCoord>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let mut ser_map = serializer.serialize_map(Some(location.iter().count()))?;
    for (axis, coord) in location.iter() {
        ser_map.serialize_entry(axis, &coord.to_f64())?;
    }
    ser_map.end()
}

pub(crate) fn string_design_location_from_map<'de, D>(
    deserializer: D,
) -> Result<IndexMap<String, DesignCoord>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let raw_map: HashMap<String, f64> = HashMap::deserialize(deserializer)?;
    let mut location = IndexMap::default();
    for (axis, value) in raw_map {
        location.insert(axis, DesignCoord::new(value));
    }
    Ok(location)
}

pub(crate) fn option_design_location_to_map<S>(
    location: &Option<DesignLocation>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    match location {
        Some(loc) => design_location_to_map(loc, serializer),
        None => serializer.serialize_none(),
    }
}
pub(crate) fn option_design_location_from_map<'de, D>(
    deserializer: D,
) -> Result<Option<DesignLocation>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let opt: Option<HashMap<String, f64>> = Option::deserialize(deserializer)?;
    match opt {
        Some(raw_map) => {
            let mut location = DesignLocation::default();
            for (axis, value) in raw_map {
                location.insert(
                    Tag::from_str(&axis).map_err(serde::de::Error::custom)?,
                    DesignCoord::new(value),
                );
            }
            Ok(Some(location))
        }
        None => Ok(None),
    }
}

pub(crate) fn default_true() -> bool {
    true
}
// pub(crate) fn default_false() -> bool {
//     false
// }
pub(crate) fn is_true(value: &bool) -> bool {
    *value
}

pub(crate) fn is_default<T>(value: &T) -> bool
where
    T: Default + PartialEq,
{
    *value == T::default()
}
