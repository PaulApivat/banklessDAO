/* ERC-20 Token Transfers */
/* Bankless DAO Treasury */
/* Join table for Token Symbol */
WITH temp_table AS (
SELECT 
    evt_tx_hash,
    evt_block_time,
    tr."from" AS address,
    -tr.value AS amount,
    contract_address
FROM erc20."ERC20_evt_Transfer" tr

UNION ALL

SELECT 
    evt_tx_hash,
    evt_block_time,
    tr."to" AS address,
    tr.value AS amount,
    contract_address
FROM erc20."ERC20_evt_Transfer" tr
), temp_table2 AS (
SELECT
    evt_tx_hash,
    evt_block_time,
    address,
    amount/10^18 AS balance,
    contract_address
FROM temp_table
WHERE address = '\xf26d1Bb347a59F6C283C53156519cC1B1ABacA51'
ORDER BY evt_block_time DESC
)
SELECT 
    evt_tx_hash,
    evt_block_time,
    address,
    balance,
    tr.contract_address,
    CASE 
        WHEN tr.contract_address = '\x2d94aa3e47d9d5024503ca8491fce9a2fb4da198' THEN 'BANK'
        ELSE tok.symbol
    END AS token
FROM temp_table2 tr
LEFT JOIN erc20.tokens tok ON tr.contract_address = tok.contract_address


/* Bankless DAO Treasury Token Holdings */
WITH temp_table AS 
    (SELECT 
    evt_tx_hash,
    evt_block_time,
    tr."from" AS address,
    -tr.value AS amount,
    contract_address
    FROM erc20."ERC20_evt_Transfer" tr

    UNION ALL

    SELECT 
    evt_tx_hash,
    evt_block_time,
    tr."to" AS address,
    tr.value AS amount,
    contract_address
    FROM erc20."ERC20_evt_Transfer" tr
    ), 

temp_table2 AS 
    (SELECT
    evt_tx_hash,
    evt_block_time,
    address,
    amount as balance,
    contract_address
    FROM temp_table
    WHERE address = '\xf26d1Bb347a59F6C283C53156519cC1B1ABacA51'
    ORDER BY evt_block_time DESC),

temp_table3 AS (
SELECT 
        tr.contract_address AS contract_addr,
        CASE 
            WHEN tr.contract_address = '\x2d94aa3e47d9d5024503ca8491fce9a2fb4da198' THEN 'BANK'
            ELSE tok.symbol
        END AS token,
        CASE 
            WHEN tr.contract_address = '\xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48'
            THEN balance/1e6 
        ELSE balance/1e18
        END AS quantity
FROM temp_table2 tr
LEFT JOIN erc20.tokens tok ON tr.contract_address = tok.contract_address
), 

temp_table4 AS (
SELECT
    contract_addr,
    token,
    SUM(quantity) AS sum_quantity
FROM temp_table3
GROUP BY 1,2
),

temp_table5 AS (
SELECT 
    DATE_TRUNC('day', minute) AS mt,
    AVG(price) AS avg_price,
    symbol
FROM prices."usd" 
WHERE symbol = 'USDC' OR (symbol = 'WETH')
GROUP BY 1,3
ORDER BY 1 DESC
LIMIT 2
)

SELECT
mt,
contract_addr,
token,
sum_quantity,
avg_price
FROM temp_table4 t FULL JOIN temp_table5 t2 ON t.token = t2.symbol