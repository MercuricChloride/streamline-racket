#lang streamline

import "interfaces/ERC721.sol";

bayc = ERC721(0xBC4CA0EdA7647A8aB7C2061c2E118A18a936f13D);

mfn doubleNum = EVENTS
    |> (num) => num * 2 + 123;