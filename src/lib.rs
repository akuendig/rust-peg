#[link(name = "peg"
, package_id = "peg"
, vers = "0.1"
, author = "Adrian Kuendig"
, uuid = "3d5ca519-5558-4ab7-8880-e7a3b62ad2c4")];
#[crate_type = "lib"];
#[warn(non_camel_case_types)];

extern mod extra;

pub mod peg;
pub mod state;
