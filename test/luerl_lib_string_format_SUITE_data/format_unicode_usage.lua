TxtSmallEng = "arvizturo tukorfurogep"
TxtSmallUtf = "árvíztűrő tükörfúrógép"
TxtUtf = "árvíztűrő tükörfúrógép _,.-+=!$?/%@(){}[]<>0123456789'\"ÁRVÍZTŰRŐ TÜKÖRFÚRÓGÉP"

return
    string.len(TxtSmallEng)
  , string.len(TxtSmallUtf)
  , TxtSmallEng:gsub("e", "_")
  , string.find(TxtSmallUtf, "tükörfúrógép")
  , ("álom"):gsub("á", "a")
    , ("alom"):gsub("a", "á") == "álom"

  --, TxtSmallEng:gsub("e", "é")
  , "no_more_test"
