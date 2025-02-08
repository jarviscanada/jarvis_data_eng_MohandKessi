package ca.jrvs.apps.stockquote;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class StockQuoteDao implements CrudDao<StockQuote, String> {

  private Connection connection;

  public StockQuoteDao(Connection connection) {
    this.connection = connection;
  }

  @Override
  public StockQuote save(StockQuote entity) throws IllegalArgumentException {
    if (entity == null || entity.getGlobalQuote() == null) {
      throw new IllegalArgumentException("StockQuote entity cannot be null.");
    }

    String sql = "INSERT INTO quote (symbol, open, high, low, price, volume, latest_trading_day, previous_close, change, change_percent) " +
        "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) " +
        "ON CONFLICT (symbol) DO UPDATE SET " +
        "open = EXCLUDED.open, high = EXCLUDED.high, low = EXCLUDED.low, " +
        "price = EXCLUDED.price, volume = EXCLUDED.volume, latest_trading_day = EXCLUDED.latest_trading_day, " +
        "previous_close = EXCLUDED.previous_close, change = EXCLUDED.change, change_percent = EXCLUDED.change_percent";

    try (PreparedStatement statement = connection.prepareStatement(sql)) {
      StockQuote.GlobalQuote globalQuote = entity.getGlobalQuote();

      statement.setString(1, globalQuote.getSymbol());
      statement.setDouble(2, globalQuote.getOpen());
      statement.setDouble(3, globalQuote.getHigh());
      statement.setDouble(4, globalQuote.getLow());
      statement.setDouble(5, globalQuote.getPrice());
      statement.setDouble(6, globalQuote.getVolume());
      statement.setDate(7, Date.valueOf(globalQuote.getLatestTradingDate()));
      statement.setDouble(8, Double.parseDouble(globalQuote.getPreviousClose()));
      statement.setDouble(9, globalQuote.getChange());
      statement.setString(10, globalQuote.getChangePercent());

      statement.executeUpdate();
      return entity;
    } catch (SQLException e) {
      throw new RuntimeException("Failed to save StockQuote: " + e.getMessage(), e);
    }
  }

  @Override
  public Optional<StockQuote> findById(String symbol) throws IllegalArgumentException {
    if (symbol == null || symbol.trim().isEmpty()) {
      throw new IllegalArgumentException("Symbol cannot be null or empty.");
    }

    String sql = "SELECT * FROM quote WHERE symbol = ?";
    try (PreparedStatement statement = connection.prepareStatement(sql)) {
      statement.setString(1, symbol);

      try (ResultSet resultSet = statement.executeQuery()) {
        if (resultSet.next()) {
          StockQuote.GlobalQuote globalQuote = new StockQuote.GlobalQuote();
          globalQuote.setSymbol(resultSet.getString("symbol"));
          globalQuote.setOpen(resultSet.getDouble("open"));
          globalQuote.setHigh(resultSet.getDouble("high"));
          globalQuote.setLow(resultSet.getDouble("low"));
          globalQuote.setPrice(resultSet.getDouble("price"));
          globalQuote.setVolume(resultSet.getDouble("volume"));
          globalQuote.setLatestTradingDate(resultSet.getDate("latest_trading_day").toString());
          globalQuote.setPreviousClose(String.valueOf(resultSet.getDouble("previous_close")));
          globalQuote.setChange(resultSet.getDouble("change"));
          globalQuote.setChangePercent(resultSet.getString("change_percent"));

          StockQuote stockQuote = new StockQuote();
          stockQuote.setGlobalQuote(globalQuote);
          return Optional.of(stockQuote);
        }
      }
    } catch (SQLException e) {
      throw new RuntimeException("Failed to find StockQuote by symbol: " + e.getMessage(), e);
    }
    return Optional.empty();
  }

  @Override
  public Iterable<StockQuote> findAll() {
    List<StockQuote> stockQuotes = new ArrayList<>();
    String sql = "SELECT * FROM quote";

    try (PreparedStatement statement = connection.prepareStatement(sql);
        ResultSet resultSet = statement.executeQuery()) {
      while (resultSet.next()) {
        StockQuote.GlobalQuote globalQuote = new StockQuote.GlobalQuote();
        globalQuote.setSymbol(resultSet.getString("symbol"));
        globalQuote.setOpen(resultSet.getDouble("open"));
        globalQuote.setHigh(resultSet.getDouble("high"));
        globalQuote.setLow(resultSet.getDouble("low"));
        globalQuote.setPrice(resultSet.getDouble("price"));
        globalQuote.setVolume(resultSet.getDouble("volume"));
        globalQuote.setLatestTradingDate(resultSet.getDate("latest_trading_day").toString());
        globalQuote.setPreviousClose(String.valueOf(resultSet.getDouble("previous_close")));
        globalQuote.setChange(resultSet.getDouble("change"));
        globalQuote.setChangePercent(resultSet.getString("change_percent"));

        StockQuote stockQuote = new StockQuote();
        stockQuote.setGlobalQuote(globalQuote);
        stockQuotes.add(stockQuote);
      }
    } catch (SQLException e) {
      throw new RuntimeException("Failed to retrieve all StockQuotes: " + e.getMessage(), e);
    }
    return stockQuotes;
  }

  @Override
  public void deleteById(String symbol) throws IllegalArgumentException {
    if (symbol == null || symbol.trim().isEmpty()) {
      throw new IllegalArgumentException("Symbol cannot be null or empty.");
    }

    String sql = "DELETE FROM quote WHERE symbol = ?";
    try (PreparedStatement statement = connection.prepareStatement(sql)) {
      statement.setString(1, symbol);
      statement.executeUpdate();
    } catch (SQLException e) {
      throw new RuntimeException("Failed to delete StockQuote by symbol: " + e.getMessage(), e);
    }
  }

  @Override
  public void deleteAll() {
    String sql = "DELETE FROM quote";
    try (PreparedStatement statement = connection.prepareStatement(sql)) {
      statement.executeUpdate();
    } catch (SQLException e) {
      throw new RuntimeException("Failed to delete all StockQuotes: " + e.getMessage(), e);
    }
  }
}