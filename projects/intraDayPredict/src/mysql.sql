create table mainContractMinuteData(
    TradingDay      DATE     NOT NULL,
    Minute          TIME NOT NULL,
    NumericExchTime decimal(15,5)  NULL,
    InstrumentID    varchar(100)   NOT NULL,
    open            decimal(15,5)  NULL,
    high            decimal(15,5)  NULL,
    low             decimal(15,5)  NULL,
    close           decimal(15,5)  NULL,
    volume          INT        NULL,
    turnover        decimal(30,5)  NULL,
    PRIMARY KEY (TradingDay, Minute, InstrumentID)
)

## -----------------------------------------------------------------------------
insert into  mainContractMinuteData
SELECT a.TradingDay,
    a.Minute,
    a.NumericExchTime,
    a.InstrumentID,
    a.OpenPrice as open,
    a.HighPrice as high,
    a.LowPrice  as low,
    a.ClosePrice as close,
    a.Volume    as volume,
    a.Turnover  as turnover
From minute as a,
  main_contract_daily as b
WHERE a.TradingDay = b.TradingDay
AND   a.InstrumentID = b.Main_contract;
