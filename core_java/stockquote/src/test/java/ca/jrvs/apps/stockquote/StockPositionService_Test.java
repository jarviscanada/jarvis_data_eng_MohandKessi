package ca.jrvs.apps.stockquote;

import ca.jrvs.apps.stockquote.dao.StockPositionDao;
import ca.jrvs.apps.stockquote.model.StockPosition;
import ca.jrvs.apps.stockquote.service.StockPositionService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

class StockPositionService_Test {

  @Mock
  private StockPositionDao stockPositionDao;

  private StockPositionService stockPositionService;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
    stockPositionService = new StockPositionService(stockPositionDao);
  }

  @Test
  void buy_NewPosition() {
    when(stockPositionDao.findById(anyString())).thenReturn(Optional.empty());
    when(stockPositionDao.save(any(StockPosition.class))).thenAnswer(invocation -> invocation.getArgument(0));

    StockPosition position = stockPositionService.buy("AAPL", 10, 150.0);

    assertNotNull(position);
    assertEquals("AAPL", position.getSymbol());
    assertEquals(10, position.getNumOfShares());
    assertEquals(1500.0, position.getValuePaid());
    verify(stockPositionDao, times(1)).save(any(StockPosition.class));
  }

  @Test
  void buy_ExistingPosition() {
    StockPosition existingPosition = new StockPosition();
    existingPosition.setSymbol("AAPL");
    existingPosition.setNumOfShares(5);
    existingPosition.setValuePaid(750.0);

    when(stockPositionDao.findById(anyString())).thenReturn(Optional.of(existingPosition));
    when(stockPositionDao.save(any(StockPosition.class))).thenAnswer(invocation -> invocation.getArgument(0));

    StockPosition position = stockPositionService.buy("AAPL", 10, 150.0);

    assertNotNull(position);
    assertEquals("AAPL", position.getSymbol());
    assertEquals(15, position.getNumOfShares());
    assertEquals(2250.0, position.getValuePaid());
    verify(stockPositionDao, times(1)).save(any(StockPosition.class));
  }

  @Test
  void sell_PositionExists() {
    when(stockPositionDao.findById(anyString())).thenReturn(Optional.of(new StockPosition()));

    stockPositionService.sell("AAPL");

    verify(stockPositionDao, times(1)).deleteById("AAPL");
  }

  @Test
  void sell_PositionDoesNotExist() {
    when(stockPositionDao.findById(anyString())).thenReturn(Optional.empty());

    assertThrows(IllegalArgumentException.class, () -> stockPositionService.sell("AAPL"));
    verify(stockPositionDao, never()).deleteById(anyString());
  }
}