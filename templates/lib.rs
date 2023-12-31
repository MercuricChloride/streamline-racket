mod pb;

use std::collections::HashMap;

use alloy_primitives::aliases::{B16, B32, B8, U1};
use alloy_primitives::{address, Address, Bytes, FixedBytes, Log};
use alloy_sol_types::abi::{Decoder, TokenSeq};
use alloy_sol_types::sol_data::Bool;
use alloy_sol_types::{SolEvent, TopicList};
use ethabi::ethereum_types::U256;
use ethereum_abi::Abi;
use hex_literal::hex;
use prost_wkt_types::{ListValue, Struct};
use serde_json::{Map, Value};
//use pb::contract::v1 as contract;
use substreams::Hex;
use substreams_database_change::pb::database::DatabaseChanges;
use substreams_ethereum::block_view::LogView;
use substreams_ethereum::pb::eth::v2::{self as eth, Block};
use substreams_ethereum::Event;

use serde::{Deserialize, Serialize, Serializer};

use substreams_alloy_helpers::{
    format_inputs, loose_sol, map_access, map_insert, map_literal, prelude::*, to_map, with_map,
};

type ValueMap = serde_json::Map<String, Value>;

include!("/tmp/streamline.rs");
