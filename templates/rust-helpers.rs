//mod abi;
mod pb;
use std::collections::HashMap;

use alloy_primitives::{Address, FixedBytes, Log};
use alloy_sol_types::abi::{Decoder, TokenSeq};
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

use struct_iterable::Iterable;

use serde::{Deserialize, Serialize, Serializer};

use alloy_sol_macro::sol;

type address = Address;

@in[c instances]{
    const @(hash-ref c 'instanceName): [u8; 20] = address!("@(hash-ref c 'address)");
}

@in[p paths]{
sol! {
    #[derive(derive_more::From, Serialize, Deserialize)]
    @p
}
}


fn block_to_alloy_events<T: SolEvent + Serialize>(
    blk: &eth::Block,
    addresses: &Vec<address>,
) -> Vec<prost_wkt_types::Value> {
    let validate = false;
    blk.alloy_logs(addresses)
        .iter()
        .filter_map(|l| T::decode_log_object(l, validate).ok())
        .map(|e| serde_json::to_string_pretty(&e).unwrap())
        .map(|e| serde_json::from_str(&e).unwrap())
        .collect()
}

trait AlloyLogs {
    fn alloy_logs(&self, addresses: &Vec<Address>) -> Vec<Log>;
}

impl AlloyLogs for eth::Block {
    fn alloy_logs(&self, addresses: &Vec<Address>) -> Vec<Log> {
        self.logs()
            .filter(|log| {
                if addresses.is_empty() {
                    true
                } else {
                    let address = &Address::from_slice(log.address());
                    addresses.contains(address)
                }
            })
            .map(|l| l.into_log())
            .collect()
    }
}

trait AlloyLog {
    fn into_log(&self) -> Log;
}

impl AlloyLog for LogView<'_> {
    fn into_log(&self) -> Log {
        let topics = self
            .topics()
            .iter()
            .map(|t| FixedBytes::try_from(&t.as_slice()[..]).unwrap())
            .collect();

        let data = self.data().to_vec().into();

        Log::new(topics, data).expect(\"Couldn't create a AlloyLog from a LogView\")
    }
}
