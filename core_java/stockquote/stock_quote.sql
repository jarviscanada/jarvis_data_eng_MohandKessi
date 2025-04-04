CREATE DATABASE stock_quote;

\c stock_quote;

CREATE TABLE public.quote (
    symbol             VARCHAR(10)       NOT NULL PRIMARY KEY,
    open               NUMERIC(10,2)     NOT NULL,
    high               NUMERIC(10,2)     NOT NULL,
    low                NUMERIC(10,2)     NOT NULL,
    price              NUMERIC(10,2)     NOT NULL,
    volume             INTEGER           NOT NULL,
    latest_trading_day DATE              NOT NULL,
    previous_close     NUMERIC(10,2)     NOT NULL,
    change             NUMERIC(10,2)     NOT NULL,
    change_percent     VARCHAR(10)       NOT NULL,
    timestamp          TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);


CREATE TABLE IF public.position (
    symbol           VARCHAR(10)   NOT NULL,
    number_of_shares INTEGER       NOT NULL,
    value_paid       NUMERIC(10,2) NOT NULL,
    PRIMARY KEY (symbol),
    CONSTRAINT symbol_fk FOREIGN KEY (symbol) REFERENCES public.quote(symbol)
);
