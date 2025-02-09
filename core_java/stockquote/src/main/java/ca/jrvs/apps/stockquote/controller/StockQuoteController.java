package ca.jrvs.apps.stockquote.controller;

import ca.jrvs.apps.stockquote.model.StockPosition;
import ca.jrvs.apps.stockquote.model.StockQuote;
import ca.jrvs.apps.stockquote.service.StockPositionService;
import ca.jrvs.apps.stockquote.service.StockQuoteService;
import java.util.Optional;
import java.util.Scanner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class StockQuoteController {

  private final StockQuoteService quoteService;
  private final StockPositionService positionService;
  private final Scanner scanner;
  private static final Logger logger = LoggerFactory.getLogger(StockQuoteController.class);

  public StockQuoteController(StockQuoteService quoteService, StockPositionService positionService) {
    this.quoteService = quoteService;
    this.positionService = positionService;
    this.scanner = new Scanner(System.in);
  }

  public void initClient() {

    logger.info("Welcome to the Stock Quote Application!");

    while (true) {
      logger.info("\nPlease choose an option:");
      logger.info("1. Fetch and display stock quote");
      logger.info("2. Buy shares");
      logger.info("3. Sell shares");
      logger.info("4. View position value");
      logger.info("5. Exit");
      logger.info("Enter your choice: ");

      String choice = scanner.nextLine();

      switch (choice) {
        case "1":
          fetchAndDisplayQuote();
          break;
        case "2":
          buyShares();
          break;
        case "3":
          sellShares();
          break;
        case "4":
          viewPositionValue();
          break;
        case "5":
          logger.info("Exiting the application. Goodbye!");
          return;
        default:
          logger.warn("Invalid choice. Please try again.");
      }
    }
  }

  private void fetchAndDisplayQuote() {
    logger.info("Enter the stock ticker symbol: ");
    String ticker = scanner.nextLine().toUpperCase();

    Optional<StockQuote> quote = quoteService.fetchQuoteDataFromAPI(ticker);

    if (quote.isPresent()) {
      logger.info("\nLatest Stock Quote for " + ticker + ":");
      logger.info(quote.toString());
    } else {
      logger.warn("Failed to fetch quote for ticker: " + ticker);
    }
  }

  private void buyShares() {
    logger.info("Enter the stock ticker symbol: ");
    String ticker = scanner.nextLine().toUpperCase();

    logger.info("Enter the number of shares to buy: ");
    int numOfShares = Integer.parseInt(scanner.nextLine());

    logger.info("Enter the price per share: ");
    double price = Double.parseDouble(scanner.nextLine());

    StockPosition position = positionService.buy(ticker, numOfShares, price);
    logger.info("\nSuccessfully bought shares:");
    logger.info(position.toString());
  }

  private void sellShares() {
    logger.info("Enter the stock ticker symbol: ");
    String ticker = scanner.nextLine().toUpperCase();

    positionService.sell(ticker);
    logger.info("\nSuccessfully sold all shares of " + ticker);
  }

  private void viewPositionValue() {
    logger.info("Enter the stock ticker symbol: ");
    String ticker = scanner.nextLine().toUpperCase();

    Optional<StockPosition> position = positionService.findById(ticker);

    if (position.isPresent()) {
      StockPosition pos = position.get();
      double currentValue = pos.getNumOfShares() * quoteService.fetchQuoteDataFromAPI(ticker)
          .orElseThrow(() -> new RuntimeException("Failed to fetch quote for ticker: " + ticker))
          .getGlobalQuote().getPrice();

      logger.info("\nPosition Details for " + ticker + ":");
      logger.info("Number of Shares: " + pos.getNumOfShares());
      logger.info("Total Amount Paid: $" + pos.getValuePaid());
      logger.info("Current Value: $" + currentValue);
      logger.info("Profit/Loss: $" + (currentValue - pos.getValuePaid()));
    } else {
      logger.warn("No position found for ticker: " + ticker);
    }
  }
}