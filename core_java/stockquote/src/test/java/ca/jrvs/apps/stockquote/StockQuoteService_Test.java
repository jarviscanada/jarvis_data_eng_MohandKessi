package ca.jrvs.apps.stockquote;

import ca.jrvs.apps.stockquote.dao.StockQuoteDao;
import ca.jrvs.apps.stockquote.model.StockQuote;
import ca.jrvs.apps.stockquote.service.StockQuoteService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

class StockQuoteService_Test {

  @Mock
  private StockQuoteDao stockQuoteDao;

  @Mock
  private QuoteHttpHelper quoteHttpHelper;

  private StockQuoteService stockQuoteService;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
    stockQuoteService = new StockQuoteService(stockQuoteDao, quoteHttpHelper);
  }

  @Test
  void fetchQuoteDataFromAPI_Success() {
    StockQuote mockQuote = new StockQuote();
    when(quoteHttpHelper.fetchQuote(anyString())).thenReturn(mockQuote);
    when(stockQuoteDao.save(mockQuote)).thenReturn(mockQuote);

    Optional<StockQuote> result = stockQuoteService.fetchQuoteDataFromAPI("AAPL");

    assertTrue(result.isPresent());
    assertEquals(mockQuote, result.get());
    verify(quoteHttpHelper, times(1)).fetchQuote("AAPL");
    verify(stockQuoteDao, times(1)).save(mockQuote);
  }

  @Test
  void fetchQuoteDataFromAPI_Failure() {
    when(quoteHttpHelper.fetchQuote(anyString())).thenThrow(new RuntimeException("API error"));

    Optional<StockQuote> result = stockQuoteService.fetchQuoteDataFromAPI("INVALID");

    assertFalse(result.isPresent());
    verify(quoteHttpHelper, times(1)).fetchQuote("INVALID");
    verify(stockQuoteDao, never()).save(any());
  }
}