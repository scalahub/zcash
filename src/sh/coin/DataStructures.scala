
package sh.coin

trait CoinTx {  
  def getHashAsString:String
  def coinSerializeHex:String 
}

trait CoinKey {
  def getAddress:String
  def getPrivateKey:String
  def getPrivateKeyBigInt:BigInt
}

trait CoinBlock {
  def getHash:String
  def getPrevBlockHash:String
  def getTime:Long
  def getVersion:Long
  def getTransactionsWithIndex:Seq[(CoinTx, Int)]
}

// below amount is in "Satoshis"
// output of a tx 
case class Out(address:String, amount:BigInt){if (amount <= 0) throw new Exception("Out amount is <= 0: "+amount)}

// input to a tx
case class In(txHash:String, vOut:Int)