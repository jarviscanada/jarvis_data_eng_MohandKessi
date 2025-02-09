package ca.jrvs.apps.stockquote.controller;

import ca.jrvs.apps.stockquote.model.StockPosition;
import ca.jrvs.apps.stockquote.model.StockQuote;
import ca.jrvs.apps.stockquote.service.StockPositionService;
import ca.jrvs.apps.stockquote.service.StockQuoteService;
import java.util.Optional;
import java.util.Scanner;

public class StockQuoteController {

  private final StockQuoteService quoteService;
  private final StockPositionService positionService;
  private final Scanner scanner;

  public StockQuoteController(StockQuoteService quoteService, StockPositionService positionService) {
    this.quoteService = quoteService;
    this.positionService = positionService;
    this.scanner = new Scanner(System.in);
  }

  public void initClient() {
    System.out.println("Welcome to the Stock Quote Application!");

    while (true) {
      System.out.println("\nPlease choose an option:");
      System.out.println("1. Fetch and display stock quote");
      System.out.println("2. Buy shares");
      System.out.println("3. Sell shares");
      System.out.println("4. View position value");
      System.out.println("5. Exit");
      System.out.print("Enter your choice: ");

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
          System.out.println("Exiting the application. Goodbye!");
          return;
        default:
          System.out.println("Invalid choice. Please try again.");
      }
    }
  }

  private void fetchAndDisplayQuote() {
    System.out.print("Enter the stock ticker symbol: ");
    String ticker = scanner.nextLine().toUpperCase();

    Optional<StockQuote> quote = quoteService.fetchQuoteDataFromAPI(ticker);

    if (quote.isPresent()) {
      System.out.println("\nLatest Stock Quote for " + ticker + ":");
      System.out.println(quote.get());
    } else {
      System.out.println("Failed to fetch quote for ticker: " + ticker);
    }
  }

  private void buyShares() {
    System.out.print("Enter the stock ticker symbol: ");
    String ticker = scanner.nextLine().toUpperCase();

    System.out.print("Enter the number of shares to buy: ");
    int numOfShares = Integer.parseInt(scanner.nextLine());

    System.out.print("Enter the price per share: ");
    double price = Double.parseDouble(scanner.nextLine());

    StockPosition position = positionService.buy(ticker, numOfShares, price);
    System.out.println("\nSuccessfully bought shares:");
    System.out.println(position);
  }

  private void sellShares() {
    System.out.print("Enter the stock ticker symbol: ");
    String ticker = scanner.nextLine().toUpperCase();

    positionService.sell(ticker);
    System.out.println("\nSuccessfully sold all shares of " + ticker);
  }

  private void viewPositionValue() {
    System.out.print("Enter the stock ticker symbol: ");
    String ticker = scanner.nextLine().toUpperCase();

    Optional<StockPosition> position = positionService.findById(ticker);

    if (position.isPresent()) {
      StockPosition pos = position.get();
      double currentValue = pos.getNumOfShares() * quoteService.fetchQuoteDataFromAPI(ticker)
          .orElseThrow(() -> new RuntimeException("Failed to fetch quote for ticker: " + ticker))
          .getGlobalQuote().getPrice();

      System.out.println("\nPosition Details for " + ticker + ":");
      System.out.println("Number of Shares: " + pos.getNumOfShares());
      System.out.println("Total Amount Paid: $" + pos.getValuePaid());
      System.out.println("Current Value: $" + currentValue);
      System.out.println("Profit/Loss: $" + (currentValue - pos.getValuePaid()));
    } else {
      System.out.println("No position found for ticker: " + ticker);
    }
  }
}