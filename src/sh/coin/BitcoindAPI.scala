package sh.coin

import util.CurlJsonData
import java.net.Authenticator
import java.net.PasswordAuthentication
import java.util.concurrent.atomic.AtomicLong

case class BitcoindBlock(hash:String, prevBlockHash:String, time:Long, version:Long, txs:Seq[(BitcoindTx, Int)]) extends CoinBlock {
  def getHash = hash
  def getPrevBlockHash = prevBlockHash
  def getTime = time
  def getVersion = version
  def getTransactionsWithIndex = txs
}
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
  private def removeDecimal(s:String) = { 
    val bd = new BigDecimal(new java.math.BigDecimal(s))
    val m = BigDecimal("1"+"0"*decimalDigits)
    val amt=bd*m
    amt.toBigInt.toString
  }
  private def insertDecimal(bigInt:BigInt) = {
    if (bigInt == null) "0" else {
      val sign = bigInt.signum
      val s = bigInt.abs.toString
      val i = s.length
      val str = if (i< (decimalDigits+1)) ("0."+"0"*(decimalDigits-i)+s) else s.substring(0, i-decimalDigits)+"."+s.substring(i-decimalDigits)
      if (sign < 0) "-"+str else str
    }
  }

  def getSoftwareVersion:String = {
    val xml = CurlJsonData.curlXML(
      rpcHost, 
      s"""{"method":"getnetworkinfo","params":[],"id":$id,"jsonrpc":"1.0"}"""
    )
    (xml \\ "subversion").text
  }
  def getConfirmations(txHash:String) = {
    val xml = CurlJsonData.curlXML(
      rpcHost, 
      s"""{"method":"getrawtransaction","params":["$txHash", 1],"id":$id,"jsonrpc":"1.0"}"""
    )
    val txids = (xml \ "result" \ "txid")
    if (txids.isEmpty) 0 else {
      val conf = xml \\ "confirmations"
      if (conf.isEmpty) 0 else {
        conf.text.toInt
      } 
    }
  }
  def pushTx(hex:String) = {
    CurlJsonData.curlXML(
      rpcHost, 
      s"""{"method":"sendrawtransaction","params":["$hex"],"id":$id,"jsonrpc":"1.0"}"""
    ) 
    "Ok"
  }

  def importAddress(address:String) = {
    val label = address
    CurlJsonData.curl(
      rpcHost, 
      s"""{"jsonrpc": "1.0", "id":$id, "method": "importaddress", "params": ["$address", "", false] }"""
    )    
    s"$id"
  }
  def testConnection = {
    CurlJsonData.curl(
      rpcHost, 
      s"""{"method":"getblockchaininfo","params":[],"id":$id,"jsonrpc":"1.0"}"""
    )
  }  
  def getTransaction(txHash:String) = {
    val xml = CurlJsonData.curlXML(
      rpcHost, 
      s"""{"method":"getrawtransaction","params":["$txHash", 1],"id":$id,"jsonrpc":"1.0"}"""
    )
    parseTxXML((xml \ "result")(0))
  }
  private def parseTxXML(txXML:scala.xml.Node) = {
    val txhash  = (txXML \ "txid").text
    val version  = (txXML \ "version").text.toInt
    val hex = (txXML \ "hex").text 
    val vOuts = (txXML \ "vout").map{vOut =>
      val value = BigInt(removeDecimal((vOut \ "value").text))
      val n = ((vOut \ "n").text).toInt
      val sp = (vOut \ "scriptPubKey")
      val outtype =  (sp \ "type").text      
      if (outtype == "pubkeyhash") {
        val reqSigs = (sp \ "reqSigs").text.toInt
        if (reqSigs == 1) {
          val address = (sp \ "addresses").text
          val dest = Out(address, value)
          Some(dest)
        } else None
      } else None
    }.zipWithIndex      
    
    val vIns = (txXML \ "vin").map{vIn => 
      val ins = (vIn \ "txid")
      if (ins.nonEmpty) {
        val txid = (vIn \ "txid").text
        val vout = ((vIn \ "vout").text).toInt
        Some(new In(txid, vout))
      } else None
    }.collect{case Some(in) => in}.zipWithIndex
    
    BitcoindTx(txhash, version, vOuts, vIns, hex)
  }
  
  def getBlock(blockHash:String):BitcoindBlock = {
    val xml = CurlJsonData.curlXML(
      rpcHost, 
      s"""{"method":"getblock","params":["$blockHash", 2],"id":$id,"jsonrpc":"1.0"}"""
    )
    val prevBlkHash =  (xml \\ "previousblockhash").text
    val version =  (xml \\ "version").text.toLong
    val time = (xml \\ "time").text.toLong * 1000
    val txs = (xml \\ "result" \\ "tx").map(parseTxXML).zipWithIndex
    BitcoindBlock(blockHash, prevBlkHash, time, version, txs)
  }
  
  def getBestBlockHash = {
    val xml = CurlJsonData.curlXML(
      rpcHost, 
      s"""{"method":"getbestblockhash","params":[],"id":$id,"jsonrpc":"1.0"}"""
    )
    (xml \\ "result").text
  }
  def getAddresses = {
    val xml = CurlJsonData.curlXML(
      rpcHost, 
      s"""{"method":"getaddressesbyaccount","params":[""],"id":$id,"jsonrpc":"1.0"}"""
    )
    
    val addresses = (xml \\ "result").map(_.text)
    addresses
  }
  
  def createRawTransaction(inputs:Array[In], dests:Array[Out]) = {
    val ins = inputs.map{
      case In(txHash, vOut) =>
        s"""{\"txid\":\"$txHash\",\"vout\":$vOut}"""
    }.reduceLeft(_+","+_)
    
    val outs = dests.map{
      case Out(addr, amtBigInt) =>
        val amtSat = insertDecimal(amtBigInt)
        s"""\"$addr\":$amtSat"""
    }.reduceLeft(_+","+_)
    
    val qry = s"""
{
  "method": "createrawtransaction", 
  "params": [
     [$ins], {$outs}
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

