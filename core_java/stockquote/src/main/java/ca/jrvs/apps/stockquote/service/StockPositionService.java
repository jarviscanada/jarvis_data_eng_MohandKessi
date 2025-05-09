package ca.jrvs.apps.stockquote.service;

import ca.jrvs.apps.stockquote.dao.StockPositionDao;
import ca.jrvs.apps.stockquote.model.StockPosition;
import java.util.Optional;

public class StockPositionService {

  private final StockPositionDao dao;

  public StockPositionService(StockPositionDao stockPositionDao) {
    this.dao = stockPositionDao;
  }

  public StockPosition buy(String ticker, int numberOfShares, double price) {
    double totalPaid = numberOfShares * price;

    Optional<StockPosition> existingPosition = dao.findById(ticker);
    StockPosition position;
    if (existingPosition.isPresent()) {
      position = existingPosition.get();
      position.setNumOfShares(position.getNumOfShares() + numberOfShares);
      position.setValuePaid(position.getValuePaid() + totalPaid);
    }else {
    // Create a new position
    position = new StockPosition();
    position.setSymbol(ticker);
    position.setNumOfShares(numberOfShares);
    position.setValuePaid(totalPaid);
  }
    return dao.save(position);
  }

  public void sell(String ticker) {
    // Fetch the existing position
    Optional<StockPosition> existingPosition = dao.findById(ticker);

    if (existingPosition.isPresent()) {
      // Delete the position from the database
      dao.deleteById(ticker);
    } else {
      throw new IllegalArgumentException("No position found for ticker: " + ticker);
    }
  }

  public Optional<StockPosition> findById(String ticker) {
    return dao.findById(ticker);
  }

}