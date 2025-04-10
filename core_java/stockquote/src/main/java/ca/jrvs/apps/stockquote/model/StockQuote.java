package ca.jrvs.apps.stockquote.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class StockQuote {

  @JsonProperty("Global Quote")
  private GlobalQuote globalQuote;

  public GlobalQuote getGlobalQuote() {
    return globalQuote;
  }

  public void setGlobalQuote(GlobalQuote globalQuote) {
    this.globalQuote = globalQuote;
  }

  public static class GlobalQuote {
    @JsonProperty("01. symbol")
    private String symbol;

    @JsonProperty("02. open")
    private double open;

    @JsonProperty("03. high")
    private double high;

    @JsonProperty("04. low")
    private double low;

    @JsonProperty("05. price")
    private double price;

    @JsonProperty("06. volume")
    private double volume;

    @JsonProperty("07. latest trading day")
    private String latestTradingDate;

    @JsonProperty("08. previous close")
    private String previousClose;

    @JsonProperty("09. change")
    private double change;

    @JsonProperty("10. change percent")
    private String changePercent;

    // Getters and setters
    public String getSymbol() {
      return symbol;
    }

    public void setSymbol(String symbol) {
      this.symbol = symbol;
    }

    public double getOpen() {
      return open;
    }

    public void setOpen(double open) {
      this.open = open;
    }

    public double getHigh() {
      return high;
    }

    public void setHigh(double high) {
      this.high = high;
    }

    public double getLow() {
      return low;
    }

    public void setLow(double low) {
      this.low = low;
    }

    public double getPrice() {
      return price;
    }

    public void setPrice(double price) {
      this.price = price;
    }

    public double getVolume() {
      return volume;
    }

    public void setVolume(double volume) {
      this.volume = volume;
    }

    public String getLatestTradingDate() {
      return latestTradingDate;
    }

    public void setLatestTradingDate(String latestTradingDate) {
      this.latestTradingDate = latestTradingDate;
    }

    public double getChange() {
      return change;
    }

    public void setChange(double change) {
      this.change = change;
    }

    public String getChangePercent() {
      return changePercent;
    }

    public void setChangePercent(String changePercent) {
      this.changePercent = changePercent;
    }

    public String getPreviousClose() {
      return previousClose;
    }

    public void setPreviousClose(String previousClose) {
      this.previousClose = previousClose;
    }

    @Override
    public String toString() {
      return "GlobalQuote{" +
          "symbol='" + symbol + '\'' +
          ", open=" + open +
          ", high=" + high +
          ", low=" + low +
          ", price=" + price +
          ", volume=" + volume +
          ", latestTradingDate='" + latestTradingDate + '\'' +
          ", previousClose='" + previousClose + '\'' +
          ", change=" + change +
          ", changePercent='" + changePercent + '\'' +
          '}';
    }
  }

  @Override
  public String toString() {
    return "StockQuote{" +
        "globalQuote=" + globalQuote +
        '}';
  }
}