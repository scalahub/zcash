package sh.coin

import util.CurlJsonData
import java.net.Authenticator
import java.net.PasswordAuthentication
import java.util.concurrent.atomic.AtomicLong

case class BitcoindTx(txHash:String, version:Int, vOuts:Seq[(Option[Out], Int)], ins:Seq[(In, Int)], hex:String) extends CoinTx {
  def getHashAsString:String = txHash
  def coinSerializeHex = hex
}

class BitcoindAPI(rpcuser:String, rpcpassword:String, rpcHost:String) {
  Authenticator.setDefault(
    new Authenticator {
      override def getPasswordAuthentication:PasswordAuthentication = {
        new PasswordAuthentication (rpcuser, rpcpassword.toCharArray)
      }
    }
  )  
  val ctr = new AtomicLong(0)
  def id = ctr.incrementAndGet
  
  private val decimalDigits = 8
  
  private def insertDecimal(bigInt:BigInt) = { // inserts decimal to a BigInt and converts result to String
    if (bigInt == null) "0" else {
      val sign = bigInt.signum
      val s = bigInt.abs.toString
      val i = s.length
      val str = if (i< (decimalDigits+1)) ("0."+"0"*(decimalDigits-i)+s) else s.substring(0, i-decimalDigits)+"."+s.substring(i-decimalDigits)
      if (sign < 0) "-"+str else str
    }
  }

  def pushTx(hex:String) = {
    CurlJsonData.curlXML(
      rpcHost, 
      s"""{"method":"sendrawtransaction","params":["$hex"],"id":$id,"jsonrpc":"1.0"}"""
    ) 
    "Ok"
  }

  def createRawTransaction(ins:Array[In], outs:Array[Out]) = {
    val in = ins.map{
      case In(txHash, vOut) =>
        s"""{\"txid\":\"$txHash\",\"vout\":$vOut}"""
    }.reduceLeft(_+","+_)
    
    val out = outs.map{
      case Out(addr, amtBigInt) =>
        val amtSat = insertDecimal(amtBigInt)
        s"""\"$addr\":$amtSat"""
    }.reduceLeft(_+","+_)
    
    val qry = s"""
{
  "method": "createrawtransaction", 
  "params": [
     [$in], {$out}
  ],
  "id":$id,
  "jsonrpc":"1.0"}"""
    
    val xml = CurlJsonData.curlXML(
      rpcHost, 
      qry
    )
    (xml \\ "result").text
  }
}

