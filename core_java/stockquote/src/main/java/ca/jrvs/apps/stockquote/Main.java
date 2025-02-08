package ca.jrvs.apps.stockquote;

import ca.jrvs.apps.stockquote.dao.StockPositionDao;
import ca.jrvs.apps.stockquote.dao.StockQuoteDao;
import ca.jrvs.apps.stockquote.model.StockQuote;
import ca.jrvs.apps.stockquote.service.StockPositionService;
import ca.jrvs.apps.stockquote.service.StockQuoteService;
import io.github.cdimascio.dotenv.Dotenv;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Optional;

public class Main {

  public static void main(String[] args) {
    Dotenv dotenv = Dotenv.load();
    String symbol = "ETH";
    String apiKey = dotenv.get("API_KEY");

    try (Connection connection = DriverManager.getConnection("jdbc:postgresql://localhost:5432/stock_quote", "postgres", "password")) {
      // Initialize DAOs and services
      StockQuoteDao stockQuoteDao = new StockQuoteDao(connection);
      QuoteHttpHelper quoteHttpHelper = new QuoteHttpHelper(apiKey);
      StockQuoteService stockQuoteService = new StockQuoteService(stockQuoteDao, quoteHttpHelper);

      StockPositionDao stockPositionDao = new StockPositionDao(connection);
      StockPositionService stockPositionService = new StockPositionService(stockPositionDao);

      // Fetch and save the stock quote using the service
      Optional<StockQuote> quote = stockQuoteService.fetchQuoteDataFromAPI(symbol);

      stockPositionService.buy("BTC", 34, 90);
        System.out.println(quote);
    } catch (SQLException e) {
      e.printStackTrace();
    }
  }
}