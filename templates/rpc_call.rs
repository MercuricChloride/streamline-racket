let rpc_call_result : @(string-join (map symbol->string path) "::") Call = (
    @in[arg args]{
   @(car arg) ::from ( @(cdr arg) ),
}
  ).into();
