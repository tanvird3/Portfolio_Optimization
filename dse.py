import pandas as pd
from bdshare import get_hist_data

def dse_hist(startdate, enddate, instruments):
    df = []
    
    for instruments in instruments: 
        stock_data = get_hist_data(startdate, enddate, instruments)
        df.append(stock_data)
        
    df = pd.concat(df)
    df = df.reset_index()
    df = df.sort_values(by = ["symbol", "date"])
    df = df[["date", "symbol", "ltp", "high", "low", "open", "close", "ycp", "trade", "value", "volume"]]
    df.columns = ["DATE", "TRADING.CODE", "LTP", "HIGH", "LOW", "OPENP", "CLOSEP", "YCP", "TRADE", "VALUE", "VOLUME"]
    cols = df.columns.drop(['DATE', 'TRADING.CODE'])
    df[cols] = df[cols].apply(pd.to_numeric, errors='coerce') 
    #df[['LTP', 'HIGH', 'LOW', 'OPENP', 'CLOSEP', 'YCP', 'TRADE', 'VALUE', 'VOLUME']] = df[['LTP', 'HIGH', 'LOW', 'OPENP', 'CLOSEP', 'YCP', 'TRADE', 'VALUE', 'VOLUME']].apply(pd.to_numeric).fillna(0).astype(float)
    df[cols] = df[cols].round(2)
    return (df)
