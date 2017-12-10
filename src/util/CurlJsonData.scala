
package util

import java.io.StringWriter
import java.net.HttpURLConnection
import java.net.URL
import org.apache.commons.io.IOUtils
import org.json.JSONObject

object CurlJsonData { // makes curl requesr with JSON input (and possibly output)
  def jsonStringToXML(s:String) = try scala.xml.XML.loadString("<JSON>"+org.json.XML.toString(new JSONObject(s))+"</JSON>") catch {
    case e:Any => <error>{e.getMessage}</error>
  }
  
  def curlXML(url:String, jsonEncodedString:String) = jsonStringToXML(curl(url, jsonEncodedString))

  /**
   * Used for closing DB connections implicitly.
   * Also used for writing / reading to files
   * @author From the book "Beginning Scala"
   */
  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
  try { f(param) } finally { param.close() }
  
  def curl(url:String, jsonEncodedString:String) = {
    val httpcon = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
    httpcon.setDoOutput(true);
    httpcon.setRequestProperty("Content-Type", "application/json");
    httpcon.setRequestProperty("Accept", "application/json");
    httpcon.setRequestMethod("POST");
    httpcon.connect;
    
    val outputBytes = jsonEncodedString.getBytes("UTF-8");
    using(httpcon.getOutputStream){os =>
      os.write(outputBytes)
    }
    //https://stackoverflow.com/questions/309424/read-convert-an-inputstream-to-a-string
    val code = httpcon.getResponseCode
    val isError = code >= 400 && code <= 500
    val resp = using{
      // using method from here: https://stackoverflow.com/a/5218279/243233
      if (isError) httpcon.getErrorStream else httpcon.getInputStream
    }{is =>
      val writer = new StringWriter;
      IOUtils.copy(is, writer, "UTF-8");
      writer.toString;
    }
    httpcon.disconnect
    if (isError) throw new Exception(s"Resp code $code. Error: ${resp.take(200)}") else resp
  }
}
