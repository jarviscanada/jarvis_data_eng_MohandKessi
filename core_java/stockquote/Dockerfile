# Use an official OpenJDK runtime as a parent image
FROM openjdk:11-jre-slim

# Set the working directory inside the container
WORKDIR /app

# Copy the JAR file into the container
COPY target/stockquote-1.0-SNAPSHOT.jar app.jar

# Copy the properties file into the container
COPY src/main/resources/properties.txt /app/properties.txt

# Copy the .env file into the container
COPY .env /app/.env

# Expose the port your application runs on (if applicable)
EXPOSE 8080

# Command to run the application
CMD ["java", "-jar", "app.jar"]