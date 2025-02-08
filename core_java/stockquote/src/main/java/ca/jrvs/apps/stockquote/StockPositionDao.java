package ca.jrvs.apps.stockquote;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class StockPositionDao implements CrudDao<StockPosition, String> {

  private Connection connection;

  public StockPositionDao(Connection connection) {
    this.connection = connection;
  }

  @Override
  public StockPosition save(StockPosition entity) throws IllegalArgumentException {
    if (entity == null || entity.getSymbol() == null) {
      throw new IllegalArgumentException("StockPosition entity or symbol cannot be null.");
    }

    String sql = "INSERT INTO position (symbol, number_of_shares, value_paid) " +
        "VALUES (?, ?, ?) " +
        "ON CONFLICT (symbol) DO UPDATE SET " +
        "number_of_shares = EXCLUDED.number_of_shares, " +
        "value_paid = EXCLUDED.value_paid";

    try (PreparedStatement statement = connection.prepareStatement(sql)) {
      statement.setString(1, entity.getSymbol());
      statement.setInt(2, entity.getNumOfShares());
      statement.setDouble(3, entity.getValuePaid());

      statement.executeUpdate();
      return entity;
    } catch (SQLException e) {
      throw new RuntimeException("Failed to save StockPosition: " + e.getMessage(), e);
    }
  }

  @Override
  public Optional<StockPosition> findById(String symbol) throws IllegalArgumentException {
    if (symbol == null || symbol.trim().isEmpty()) {
      throw new IllegalArgumentException("Symbol cannot be null or empty.");
    }

    String sql = "SELECT * FROM position WHERE symbol = ?";
    try (PreparedStatement statement = connection.prepareStatement(sql)) {
      statement.setString(1, symbol);

      try (ResultSet resultSet = statement.executeQuery()) {
        if (resultSet.next()) {
          StockPosition position = new StockPosition();
          position.setSymbol(resultSet.getString("symbol"));
          position.setNumOfShares(resultSet.getInt("number_of_shares"));
          position.setValuePaid(resultSet.getDouble("value_paid"));
          return Optional.of(position);
        }
      }
    } catch (SQLException e) {
      throw new RuntimeException("Failed to find StockPosition by symbol: " + e.getMessage(), e);
    }
    return Optional.empty();
  }

  @Override
  public Iterable<StockPosition> findAll() {
    List<StockPosition> positions = new ArrayList<>();
    String sql = "SELECT * FROM position";

    try (PreparedStatement statement = connection.prepareStatement(sql);
        ResultSet resultSet = statement.executeQuery()) {
      while (resultSet.next()) {
        StockPosition position = new StockPosition();
        position.setSymbol(resultSet.getString("symbol"));
        position.setNumOfShares(resultSet.getInt("number_of_shares"));
        position.setValuePaid(resultSet.getDouble("value_paid"));
        positions.add(position);
      }
    } catch (SQLException e) {
      throw new RuntimeException("Failed to retrieve all StockPositions: " + e.getMessage(), e);
    }
    return positions;
  }

  @Override
  public void deleteById(String symbol) throws IllegalArgumentException {
    if (symbol == null || symbol.trim().isEmpty()) {
      throw new IllegalArgumentException("Symbol cannot be null or empty.");
    }

    String sql = "DELETE FROM position WHERE symbol = ?";
    try (PreparedStatement statement = connection.prepareStatement(sql)) {
      statement.setString(1, symbol);
      statement.executeUpdate();
    } catch (SQLException e) {
      throw new RuntimeException("Failed to delete StockPosition by symbol: " + e.getMessage(), e);
    }
  }

  @Override
  public void deleteAll() {
    String sql = "DELETE FROM position";
    try (PreparedStatement statement = connection.prepareStatement(sql)) {
      statement.executeUpdate();
    } catch (SQLException e) {
      throw new RuntimeException("Failed to delete all StockPositions: " + e.getMessage(), e);
    }
  }
}