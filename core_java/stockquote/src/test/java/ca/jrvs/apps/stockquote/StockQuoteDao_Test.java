package ca.jrvs.apps.stockquote;

import ca.jrvs.apps.stockquote.dao.StockQuoteDao;
import ca.jrvs.apps.stockquote.model.StockQuote;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

class StockQuoteDao_Test {

  @Mock
  private Connection connection;

  @Mock
  private PreparedStatement preparedStatement;

  @Mock
  private ResultSet resultSet;

  private StockQuoteDao dao;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
    dao = new StockQuoteDao(connection);
  }

  @Test
  void save() throws SQLException {
    StockQuote.GlobalQuote globalQuote = new StockQuote.GlobalQuote();
    globalQuote.setSymbol("AAPL");
    globalQuote.setOpen(150.0);
    globalQuote.setHigh(155.0);
    globalQuote.setLow(149.0);
    globalQuote.setPrice(152.0);
    globalQuote.setVolume(1000000);
    globalQuote.setLatestTradingDate("2023-10-01");
    globalQuote.setPreviousClose("151.0");
    globalQuote.setChange(1.0);
    globalQuote.setChangePercent("0.66%");

    StockQuote stockQuote = new StockQuote();
    stockQuote.setGlobalQuote(globalQuote);

    when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
    when(preparedStatement.executeUpdate()).thenReturn(1);

    StockQuote savedQuote = dao.save(stockQuote);

    assertNotNull(savedQuote);
    assertEquals("AAPL", savedQuote.getGlobalQuote().getSymbol());
    verify(preparedStatement, times(1)).executeUpdate();
  }

  @Test
  void findById() throws SQLException {
    when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
    when(preparedStatement.executeQuery()).thenReturn(resultSet);
    when(resultSet.next()).thenReturn(true);
    when(resultSet.getString("symbol")).thenReturn("AAPL");
    when(resultSet.getDouble("open")).thenReturn(150.0);
    when(resultSet.getDouble("high")).thenReturn(155.0);
    when(resultSet.getDouble("low")).thenReturn(149.0);
    when(resultSet.getDouble("price")).thenReturn(152.0);
    when(resultSet.getDouble("volume")).thenReturn(1000000.0);
    when(resultSet.getDate("latest_trading_day")).thenReturn(java.sql.Date.valueOf("2023-10-01"));
    when(resultSet.getDouble("previous_close")).thenReturn(151.0);
    when(resultSet.getDouble("change")).thenReturn(1.0);
    when(resultSet.getString("change_percent")).thenReturn("0.66%");

    Optional<StockQuote> foundQuote = dao.findById("AAPL");

    assertTrue(foundQuote.isPresent());
    assertEquals("AAPL", foundQuote.get().getGlobalQuote().getSymbol());
    verify(preparedStatement, times(1)).executeQuery();
  }

  @Test
  void findAll() throws SQLException {
    when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
    when(preparedStatement.executeQuery()).thenReturn(resultSet);
    when(resultSet.next()).thenReturn(true, false); // Simulate one row in the result set
    when(resultSet.getString("symbol")).thenReturn("AAPL");
    when(resultSet.getDouble("open")).thenReturn(150.0);
    when(resultSet.getDouble("high")).thenReturn(155.0);
    when(resultSet.getDouble("low")).thenReturn(149.0);
    when(resultSet.getDouble("price")).thenReturn(152.0);
    when(resultSet.getDouble("volume")).thenReturn(1000000.0);
    when(resultSet.getDate("latest_trading_day")).thenReturn(java.sql.Date.valueOf("2023-10-01"));
    when(resultSet.getDouble("previous_close")).thenReturn(151.0);
    when(resultSet.getDouble("change")).thenReturn(1.0);
    when(resultSet.getString("change_percent")).thenReturn("0.66%");

    Iterable<StockQuote> quotes = dao.findAll();

    assertNotNull(quotes);
    quotes.forEach(quote -> assertEquals("AAPL", quote.getGlobalQuote().getSymbol()));
    verify(preparedStatement, times(1)).executeQuery();
  }

  @Test
  void deleteById() throws SQLException {
    when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
    when(preparedStatement.executeUpdate()).thenReturn(1);

    dao.deleteById("AAPL");

    verify(preparedStatement, times(1)).executeUpdate();
  }

  @Test
  void deleteAll() throws SQLException {
    when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
    when(preparedStatement.executeUpdate()).thenReturn(1);

    dao.deleteAll();

    verify(preparedStatement, times(1)).executeUpdate();
  }
}