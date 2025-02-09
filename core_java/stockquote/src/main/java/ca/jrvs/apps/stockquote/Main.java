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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Main {

  private static final Logger logger = LoggerFactory.getLogger(Main.class);


  public static void main(String[] args) {
    // Load environment variables
    Dotenv dotenv = Dotenv.load();
    String apiKey = dotenv.get("API_KEY");

    // Load database connection properties
    String dbClass = PropertyLoader.getProperty("db-class");
    String server = PropertyLoader.getProperty("server");
    String database = PropertyLoader.getProperty("database");
    String port = PropertyLoader.getProperty("port");
    String username = PropertyLoader.getProperty("username");
    String password = PropertyLoader.getProperty("password");

    // Construct the JDBC URL
    String jdbcUrl = "jdbc:postgresql://" + server + ":" + port + "/" + database;

    try (Connection connection = DriverManager.getConnection(jdbcUrl, username, password)) {
      logger.info("Successfully connected to the database.");

      // Initialize DAOs
      StockQuoteDao stockQuoteDao = new StockQuoteDao(connection);
      StockPositionDao stockPositionDao = new StockPositionDao(connection);

      // Initialize services
      QuoteHttpHelper quoteHttpHelper = new QuoteHttpHelper(apiKey);
      StockQuoteService stockQuoteService = new StockQuoteService(stockQuoteDao, quoteHttpHelper);
      StockPositionService stockPositionService = new StockPositionService(stockPositionDao);

      // Initialize and start the controller
      StockQuoteController controller = new StockQuoteController(stockQuoteService, stockPositionService);
      controller.initClient();
    } catch (SQLException e) {
      logger.error("Failed to connect to the database.", e);
    }
  }
}