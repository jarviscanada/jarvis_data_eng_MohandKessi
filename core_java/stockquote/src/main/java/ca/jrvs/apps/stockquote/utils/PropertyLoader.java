package ca.jrvs.apps.stockquote.utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class PropertyLoader {

  private static final String PROPERTIES_FILE = "properties.txt";
  private static final Properties properties;

  static {
    properties = new Properties();
    try (InputStream input = PropertyLoader.class.getClassLoader().getResourceAsStream(PROPERTIES_FILE)) {
      if (input == null) {
        throw new RuntimeException("Unable to find " + PROPERTIES_FILE);
      }
      properties.load(input);
    } catch (IOException e) {
      throw new RuntimeException("Failed to load properties file", e);
    }
  }

  public static String getProperty(String key) {
    return properties.getProperty(key);
  }
}