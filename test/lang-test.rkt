#lang reader "reader.rkt"

import "interfaces/ERC721.sol";

bayc = ERC721(0xBC4CA0EdA7647A8aB7C2061c2E118A18a936f13D);
milady = ERC721(0x5Af0D9827E0c53E4799BB226655A1de152A425a5);
azuki = ERC721(0xED5AF388653567Af2F388E6224dC7C4b3241C544);
all = ERC721(0xED5AF388653567Af2F388E6224dC7C4b3241C544);

fn doubleNum = [num]
   |> (num) => num * 2;

@var from = fromAddress
fn greeter = [token, fromAddress]
   |> (token, add) => "Hey there, " + token + "! It's great to greet you!";
   |> (str) => str + " Sorry that: " + from + " fuckin sold you like that!";

mfn miladyTransfers = EVENTS
    |> (events) => events.milady.Transfer;
    |> map (transfer) => greeter(transfer._tokenId, transfer._from);

mfn baycTransfers = EVENTS
    |> (events) => events.bayc.Transfer;

@immutable
sfn storeAzukiOwners = EVENTS
    |> (events) => events.azuki.Transfer;
    |> map (t) => set(t._tokenId, t._to);

@var foo = 0
mfn mapAzukiOwnerChanges = [storeAzukiOwners.deltas]
    |> map (delta) => do{
       foo = foo + 1;
       {
        delta: delta,
        updatesThisBlock: string(foo)
       }
    };

mfn azukiTransfers = EVENTS
    |> (events) => events.azuki.Transfer;
    |> map (transfer) => transfer._tokenId + 42 + 12 - 3;
