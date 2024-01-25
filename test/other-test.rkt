#lang streamline

import "interfaces/ERC721.sol";

bayc = ERC721(0xBC4CA0EdA7647A8aB7C2061c2E118A18a936f13D);

@var foo = 0
mfn baycTransfers = EVENTS
    |> (events) => events.bayc.Transfer;

@var foo = 0
sfn baycStore = EVENTS
    |> (events) => events.bayc.Transfer;

fn doubleNum = num
    |> (num) => num * 2;