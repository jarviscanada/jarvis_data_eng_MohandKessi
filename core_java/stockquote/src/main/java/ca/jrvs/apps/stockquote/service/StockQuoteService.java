package ca.jrvs.apps.stockquote.service;

import ca.jrvs.apps.stockquote.QuoteHttpHelper;
import ca.jrvs.apps.stockquote.dao.StockQuoteDao;
import ca.jrvs.apps.stockquote.model.StockQuote;
import java.util.Optional;

public class StockQuoteService {
  private StockQuoteDao stockQuoteDao;
  private QuoteHttpHelper quoteHttpHelper;

  public StockQuoteService(StockQuoteDao stockQuoteDao, QuoteHttpHelper quoteHttpHelper) {
    this.stockQuoteDao = stockQuoteDao;
    this.quoteHttpHelper = quoteHttpHelper;

  }

  /**
   * Fetches latest quote data from endpoint
   * @param ticker
   * @return Latest quote information or empty optional if ticker symbol not found
   */
  public Optional<StockQuote> fetchQuoteDataFromAPI(String ticker) {
    try {
      // Fetch the stock quote using the QuoteHttpHelper
      StockQuote quote = quoteHttpHelper.fetchQuote(ticker);
      stockQuoteDao.save(quote);

      // Return the quote wrapped in an Optional
      return Optional.of(quote);
    } catch (Exception e) {
      // Log the error (you can use a logging framework like SLF4J)
      System.err.println("Failed to fetch quote for ticker: " + ticker + ". Error: " + e.getMessage());

      // Return an empty Optional if an error occurs
      return Optional.empty();
    }
  }

}


