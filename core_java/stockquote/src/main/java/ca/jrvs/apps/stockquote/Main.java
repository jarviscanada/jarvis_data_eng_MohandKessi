package ca.jrvs.apps.stockquote;

import ca.jrvs.apps.stockquote.controller.StockQuoteController;
import ca.jrvs.apps.stockquote.dao.StockPositionDao;
import ca.jrvs.apps.stockquote.dao.StockQuoteDao;
import ca.jrvs.apps.stockquote.service.StockPositionService;
import ca.jrvs.apps.stockquote.service.StockQuoteService;
import ca.jrvs.apps.stockquote.utils.PropertyLoader;
import io.github.cdimascio.dotenv.Dotenv;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class Main {

  public static void main(String[] args) {
    Dotenv dotenv = Dotenv.load();
    String apiKey = dotenv.get("API_KEY");

    String server = PropertyLoader.getProperty("server");
    String database = PropertyLoader.getProperty("database");
    String port = PropertyLoader.getProperty("port");
    String username = PropertyLoader.getProperty("username");
    String password = PropertyLoader.getProperty("password");
    String jdbcUrl = "jdbc:postgresql://" + server + ":" + port + "/" + database;

    try (Connection connection = DriverManager.getConnection(jdbcUrl, username, password)) {
      // Initialize DAOs and services
      StockQuoteDao stockQuoteDao = new StockQuoteDao(connection);
      QuoteHttpHelper quoteHttpHelper = new QuoteHttpHelper(apiKey);
      StockQuoteService stockQuoteService = new StockQuoteService(stockQuoteDao, quoteHttpHelper);
      StockPositionDao stockPositionDao = new StockPositionDao(connection);
      StockPositionService stockPositionService = new StockPositionService(stockPositionDao);

      StockQuoteController stockQuoteController = new StockQuoteController(stockQuoteService, stockPositionService);
      stockQuoteController.initClient();
    } catch (SQLException e) {
      e.printStackTrace();
    }
  }
}