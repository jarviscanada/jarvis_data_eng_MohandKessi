package ca.jrvs.apps.stockquote;

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

class StockPositionDao_Test {

  @Mock
  private Connection connection;

  @Mock
  private PreparedStatement preparedStatement;

  @Mock
  private ResultSet resultSet;

  private StockPositionDao dao;

  @BeforeEach
  void setUp() throws SQLException {
    MockitoAnnotations.openMocks(this);
    dao = new StockPositionDao(connection);

    when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
    when(preparedStatement.executeQuery()).thenReturn(resultSet);
  }

  @Test
  void save() throws SQLException {
    StockPosition position = new StockPosition();
    position.setSymbol("AAPL");
    position.setNumOfShares(100);
    position.setValuePaid(15000.0);

    when(preparedStatement.executeUpdate()).thenReturn(1);

    StockPosition savedPosition = dao.save(position);

    assertNotNull(savedPosition);
    assertEquals("AAPL", savedPosition.getSymbol());
    verify(preparedStatement, times(1)).executeUpdate();
  }

  @Test
  void findById() throws SQLException {
    when(resultSet.next()).thenReturn(true);
    when(resultSet.getString("symbol")).thenReturn("AAPL");
    when(resultSet.getInt("number_of_shares")).thenReturn(100);
    when(resultSet.getDouble("value_paid")).thenReturn(15000.0);

    // Act
    Optional<StockPosition> foundPosition = dao.findById("AAPL");

    // Assert
    assertTrue(foundPosition.isPresent());
    assertEquals("AAPL", foundPosition.get().getSymbol());
    verify(preparedStatement, times(1)).executeQuery();
  }

  @Test
  void deleteById() throws SQLException {
    when(preparedStatement.executeUpdate()).thenReturn(1);

    dao.deleteById("AAPL");

    verify(preparedStatement, times(1)).executeUpdate();
  }

  @Test
  void deleteAll() throws SQLException {
    when(preparedStatement.executeUpdate()).thenReturn(1);

    dao.deleteAll();

    verify(preparedStatement, times(1)).executeUpdate();
  }
}