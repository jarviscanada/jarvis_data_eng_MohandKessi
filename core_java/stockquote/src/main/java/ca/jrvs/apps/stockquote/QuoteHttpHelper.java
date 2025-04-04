package ca.jrvs.apps.stockquote;

import ca.jrvs.apps.stockquote.model.StockQuote;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

public class QuoteHttpHelper {
  private String apiKey;
  private OkHttpClient client;

  public QuoteHttpHelper(String apiKey) {
    this.apiKey = apiKey;
    this.client = new OkHttpClient();

  }

  public StockQuote fetchQuote(String symbol) throws IllegalArgumentException {
    if(symbol == null || symbol.isEmpty()) {
      throw new IllegalArgumentException("symbol cannot be null or empty");
    }

    String url = "https://alpha-vantage.p.rapidapi.com/query?function=GLOBAL_QUOTE&symbol=" + symbol + "&datatype=json";

    Request request = new Request.Builder()
        .url(url)
        .addHeader("X-RapidAPI-Key", apiKey)
        .addHeader("X-RapidAPI-Host", "alpha-vantage.p.rapidapi.com")
        .build();
    try (Response response = client.newCall(request).execute()) {
      if (!response.isSuccessful()) {
        throw new IOException("Unexpected response code: " + response.code());
      }

      String json = response.body().string();
      ObjectMapper mapper = new ObjectMapper();
      return mapper.readValue(json, StockQuote.class);
    } catch (IOException e) {
      throw new RuntimeException("Failed to fetch stock quote for symbol: " + symbol, e);
    }
  }

}
