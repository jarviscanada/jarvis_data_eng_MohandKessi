package ca.jrvs.apps.stockquote;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.github.cdimascio.dotenv.Dotenv;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import java.io.IOException;

public class Main {

  public static void main(String[] args) {
    Dotenv dotenv = Dotenv.load();
    String symbol = "MSFT";
    String apiKey = dotenv.get("API_KEY");
    QuoteHttpHelper helper = new QuoteHttpHelper(apiKey);
    System.out.println(helper.fetchQuote(symbol));
  }
}
