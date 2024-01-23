#lang streamline

import "interfaces/ERC721.sol";

milady = ERC721(0x5Af0D9827E0c53E4799BB226655A1de152A425a5);

mfn miladyTransfers = EVENTS
    |> (events) => events.milady.Transfer;
    |> map (transfer) => greeter(transfer._tokenId, transfer._from);
