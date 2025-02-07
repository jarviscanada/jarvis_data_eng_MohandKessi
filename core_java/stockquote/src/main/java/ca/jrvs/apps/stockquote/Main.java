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
    ObjectMapper mapper = new ObjectMapper();
    String symbol = "MSFT";
    String apiKey = dotenv.get("API_KEY");

    String url = "https://alpha-vantage.p.rapidapi.com/query?function=GLOBAL_QUOTE&symbol=" + symbol + "&datatype=json";

    try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
      HttpGet request = new HttpGet(url);
      request.addHeader("X-RapidAPI-Key", apiKey);
      request.addHeader("X-RapidAPI-Host", "alpha-vantage.p.rapidapi.com");

      try (CloseableHttpResponse response = httpClient.execute(request)) {
        String json = EntityUtils.toString(response.getEntity());
        System.out.println(json);

        StockQuote quote = mapper.readValue(json, StockQuote.class);
        System.out.println(quote);
        System.out.println(quote.getGlobalQuote().getHigh());
      }
    } catch (JsonMappingException e) {
      e.printStackTrace();
    } catch (JsonProcessingException e) {
      e.printStackTrace();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
