#lang streamline

import "interfaces/ERC721.sol";

bayc = ERC721(0xBC4CA0EdA7647A8aB7C2061c2E118A18a936f13D);

@var foo = 123
fn addVals = [a, b]
    |> (a, b) => do{
       foo = 69;
       foo + a + b
};

mfn doubleNum = EVENTS
    |> (num) => num * 2;

sfn tripleNum = doubleNum
    |> (num) => num * 3;

fn quadrupleNum = num
    |> (num) => num * 4;