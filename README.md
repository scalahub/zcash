# ZCash-Scala

This is a library for creating and signing ZCash transactions in Scala (and thus, Java)

For creating transactions, we use zcashd. For signing, we use Bitcoinj.

Please see https://github.com/scalahub/zcash/blob/master/src/sh/coin/zec/ZCashUtil.scala for examples on usage.

Some examples:


    val key1 = getKeyFromInt(BigInt("101002020202").bigInteger)
    val key2 = getKeyFromInt(BigInt("101002020203").bigInteger)
    val key3 = getKeyFromInt(BigInt("201002020203").bigInteger)

    val addr1 = key1.getAddress // "tmUPGRS27hPAzmRJvp7MxEPzcWLkSPGAtFR" (testnet)
    val addr2 = key2.getAddress // "tmMKum67DryZKcPaqjmSGv2CERLFriF3zei" (testnet)
    val addr3 = key3.getAddress // "tmGL4Hjcc8J1rkikD7jQqRvQvrpKtq7GbJW" (testnet)
  
    ZcashdAPI.importAddress(addr1)
    ZcashdAPI.importAddress(addr2)
    ZcashdAPI.importAddress(addr3)
  
    val in1 = In("63fbf20299883b087f1e47b81880ae5cd43150d56dce5b69d5eac1b669450a30", 0)
    val in2 = In("b2b7217210316e680ecd5ebbe38b6debfc1e2486f6f9a1cca6bdfe34ff32b274", 1)
  
    val out1 = Out(addr2, 89900000)    // last param is "Satoshis" to send
    val out2 = Out(addr3, 189900000)
  
    val tx = createGenericTx(
       Array(in1, in2), 
       Array(out1, out2)
    ) 
    
    signGenericTx(tx, Array(key2, key2)) // as many keys as inputs, keys should correspond to inputs
  
    ZcashdAPI.pushTx(tx.coinSerializeHex)