package ca.jrvs.apps.stockquote;

import ca.jrvs.apps.stockquote.dao.StockQuoteDao;
import ca.jrvs.apps.stockquote.model.StockQuote;
import io.github.cdimascio.dotenv.Dotenv;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class Main {

  public static void main(String[] args) {
    Dotenv dotenv = Dotenv.load();
    String symbol = "ENS";
    String apiKey = dotenv.get("API_KEY");
    try (Connection connection = DriverManager.getConnection("jdbc:postgresql://localhost:5432/stock_quote", "postgres", "password")) {
      StockQuoteDao dao = new StockQuoteDao(connection);
      QuoteHttpHelper helper = new QuoteHttpHelper(apiKey);
      StockQuote quote = helper.fetchQuote(symbol);
      dao.save(quote);
    }catch(SQLException e){
      e.printStackTrace();
    }

  }
}
