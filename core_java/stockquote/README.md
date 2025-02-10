# Stock Quote Application
## Introduction
This application allows users to interact with stock market data in real-time. It integrates with the Alpha Vantage API to fetch stock quotes, allows users to manage stock positions (buy/sell shares), and provides detailed information about the current value of their holdings. The app uses a PostgresSQL database to store stock position data, and the use of Docker ensures easy setup and deployment.

## Steps to Get Started

### 1. Pull the Docker Image
First, pull the StockQuote Docker image from Docker Hub:
```bash
docker pull volta808/stockquote
```

### 2. Configure `properties.txt`
Next, update the `properties.txt` file with the correct configuration details for your setup.

### 3. Add Your Alpha Vantage API Key
Put your **Alpha Vantage API Key** in the `.env` file. Make sure the file is properly configured to access the API.

### 4. Set Up a PostgreSQL Server
Set up your PostgreSQL server if you haven't already. Ensure it's running and accessible.

### 5. Create the Database
Create the database by running the following command:
```bash
psql -U postgres -f stockquote.sql
```

### 6. Run the Docker Container
Finally, run the StockQuote service in a Docker container with this command:
```bash
docker run -p 8080:8080 volta808/stockquote
```

The service will now be accessible at `http://localhost:8080`.

---

## Additional Notes
- Ensure your PostgreSQL server is properly configured to allow connections from the Docker container.
- The `properties.txt` and `.env` files must be correctly configured for the service to work as expected.


## Implementation
### ER Diagram
//TODO

### Design Pattern
This application implements the DAO (Data Access Object) and Repository design patterns. We used the DAO pattern to abstract and encapsulate all interactions with the database. It provides an interface for the service layer to interact with the data source without directly depending on the underlying data access logic.

## Test
1. **Unit Tests:**
    - DAO layer methods were tested using mock data and unit testing frameworks like JUnit.
    - Mocking was used for the database connection to simulate different scenarios (e.g., invalid stock symbol, empty database).

2. **Integration Tests:**
    - Real database tests were run by setting up a PostgreSQL instance using Docker, with actual interactions being tested to ensure the app functions correctly.
    - We tested features like adding/removing stocks from positions and retrieving quotes.

3. **End-to-End Tests:**
    - Simulated end-to-end usage, such as fetching stock quotes, placing a buy order, and viewing the portfolio, to ensure the application’s flow functions as expected.
