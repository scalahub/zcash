package sh.coin.zec;

import sh.coin._
import util._
import java.math.BigInteger
import org.bitcoinj.script.ScriptBuilder
import scala.collection.JavaConversions._
import org.bitcoinj.core.DumpedPrivateKey
import org.bitcoinj.core.ECKey
import org.bitcoinj.crypto.TransactionSignature
import org.bitcoinj.params.MainNetParams
import org.bitcoinj.params.TestNet3Params
import org.bitcoinj.core.{Transaction => BitjTx}
import scala.collection.JavaConversions._

// zcashd is similar to bitcoind in terms of t-addresses
object ZcashdAPI extends BitcoindAPI("username", "password", "http://localhost:9332") // last param is RPC host address

object ZCashUtil {    

  var isProdNet = true // set to false for testnet
  
  def getNetParam = if (isProdNet) MainNetParams.get else TestNet3Params.get    
  
  // following based on zcash implementers guide
  // wif = "Wallet Import Format"
  def isCompressedPrvKey(wif:String) = 
    if (isProdNet) wif.startsWith("L") || wif.startsWith("K")
    else wif.startsWith("c")
  
  def getKeyFromWif(wif:String) = ZCashKey(new DumpedPrivateKey(getNetParam, wif).getKey)
  def getKeyFromInt(int:java.math.BigInteger) = ZCashKey(ECKey.fromPrivate(int, false))
  def getKeyFromIntCompressed(int:java.math.BigInteger) = ZCashKey(ECKey.fromPrivate(int, true))
  
  private val prodNetVersionBytes = Array((0x1C).toByte, (0xB8).toByte) 
  private val testNetVersionBytes = Array((0x1D).toByte, (0x25).toByte)

  val versionBytes = if (isProdNet) prodNetVersionBytes else testNetVersionBytes

  private val prodNetVersionBytesStr = "1CB8"
  private val testNetVersionBytesStr = "1D25"

  val versionBytesStr = if (isProdNet) prodNetVersionBytesStr else testNetVersionBytesStr  
    
  def isValidAddress(address:String) = {
    val decoded = Hex.encodeBytes(Base58Check.decode(address))
    decoded.startsWith(versionBytesStr) && decoded.size == 44
  }
  
  def isMainNetAddress(address:String) = {
    val decoded = Hex.encodeBytes(Base58Check.decode(address))
    decoded.startsWith(prodNetVersionBytesStr) && decoded.size == 44
  }
  
  // below call requires zcashd running  
  def getCoinTxFromHex(hex:String):CoinTx = ZCashTx(new BitjTx(getNetParam, Hex.decode(hex)))
  
  // following returns HEX encoded tx
  def createGenericTxHex(ins:Array[In], outs:Array[Out]):String = ZcashdAPI.createRawTransaction(ins, outs)
  
  def createGenericTx(ins:Array[In], outs:Array[Out]) = getCoinTxFromHex(createGenericTxHex(ins, outs))
  
  def signGenericTx[T <: CoinKey](tx:CoinTx, keys:Array[T]): CoinTx = tx match {
    case ZCashTx(t) => 
      val inputs=t.getInputs
      val sigs=inputs.zipWithIndex.map(x => {
          val (input, i) = (x._1, x._2)
          val key = keys.apply(i)
          key match {
            case ZCashKey(k) => 
              val connectedPubKeyScript = ScriptBuilder.createOutputScript(k.toAddress(getNetParam));
              val hash=t.hashForSignature(i, connectedPubKeyScript, BitjTx.SigHash.ALL, false);
              (k.sign(hash), (BitjTx.SigHash.ALL.ordinal+1)|(0)); 
          }
        }
      )
      inputs.indices.foreach(i => {
          keys.apply(i) match {
            case ZCashKey(key) => 
              inputs(i).setScriptSig(ScriptBuilder.createInputScript(new TransactionSignature(sigs(i)._1, BitjTx.SigHash.ALL, false), key));
          }
        }
      )
      ZCashTx(t)
    case any => throw new Exception(s"Unsupported TxType: $any")
  }

}

import ZCashUtil._

case class ZCashTx(tx:BitjTx) extends CoinTx {
  def getHashAsString: String = tx.getHashAsString
  def coinSerializeHex = Hex.encodeBytes(tx.bitcoinSerialize)
}

case class ZCashKey(key:ECKey) extends CoinKey { 
  def getAddress = Base58Check.encode(versionBytes++key.getPubKeyHash)    
  def getPrivateKeyBigInt = key.getPrivKey
  def getPrivateKey = key.getPrivateKeyEncoded(getNetParam).toString
  def signMessage(m:String) = key.signMessage(m)
}

object ZCashTest extends App {
  
  val key1 = getKeyFromInt(BigInt("101002020202").bigInteger)
  val key2 = getKeyFromInt(BigInt("101002020203").bigInteger)
  val key3 = getKeyFromInt(BigInt("201002020203").bigInteger)
  val addr1 = key1.getAddress // "tmUPGRS27hPAzmRJvp7MxEPzcWLkSPGAtFR" (testnet)
  val addr2 = key2.getAddress // "tmMKum67DryZKcPaqjmSGv2CERLFriF3zei" (testnet)
  val addr3 = key3.getAddress // "tmGL4Hjcc8J1rkikD7jQqRvQvrpKtq7GbJW" (testnet
  
  val in1 = In("63fbf20299883b087f1e47b81880ae5cd43150d56dce5b69d5eac1b669450a30", 0)
  val in2 = In("b2b7217210316e680ecd5ebbe38b6debfc1e2486f6f9a1cca6bdfe34ff32b274", 1)
  
  ZcashdAPI.importAddress(addr1)
  ZcashdAPI.importAddress(addr2)
  ZcashdAPI.importAddress(addr3)
  
  val out1 = Out(addr2, 89900000)
  val out2 = Out(addr3, 189900000)
  
  val tx = createGenericTx(Array(in1, in2), Array(out1, out2)) // last param is "Satoshis" to send
  
  println("UNSIGNED HEX tx = "+tx.coinSerializeHex)
  
  signGenericTx(tx, Array(key2))
  
  println("SIGNED HEX tx = "+tx.coinSerializeHex)
  
  ZcashdAPI.pushTx(tx.coinSerializeHex)

  // Below example to generate bitcoinj Tx from hex
  ZCashUtil.getCoinTxFromHex("""01000000034fe292155fb8491790dbd4d26b9200e02bdebc23240428d81f36454720fd50250000000000ffffffff74b232ff34febda6cca1f9f686241efceb6d8be3bb5ecd0e686e31107221b7b20100000000ffffffff19e7b1ed3a868f1a4259cabc61b92709483355bf3e9505dc08dc373cc1d992470200000000ffffffff02a0860100000000001976a914ccce63775e2982ce3da3b4b881eabfa43d96c3e588ac400d0300000000001976a914489180dd92291aebf87353ad17b286210e4100bd88ac00000000""")
  
    
}
  
