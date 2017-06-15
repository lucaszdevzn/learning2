# MySQL 数据库


本文件夹内包含用于处理数据的各类脚本文件.

- ChinaFuturesTickData
    - `myCTP/recorder.py`: 从 `CTP` 接收得到的 `tick data`, 保存为 `csv` 格式的文件.

- ChinaFuturesBar
    - minute: 分钟级别的数据
    - daily: 日频级别的数据
    - mainContract: 主力合约
    - oiRank: 期货公式持仓排名

- ChinaFuturesInfo
    - CommissionRateInfo: 期货公司要求的各种费率
    - InstrumentInfo: 合约信息, 包括合约乘数, 最小价格变动单位(priceTick)
    - VolumeMultiple: 合约乘数, 包含历史的数据

- ChinaFuturesHFT
    - CiticPublic
    - breakTime

- trade
    - positionInfo: 持仓信息
    - tradingInfo: 交易记录
    - failedInfo: 未成交记录
    - report_account: 账户净值播报
    - report_position: 账户各持仓合约信息播报
