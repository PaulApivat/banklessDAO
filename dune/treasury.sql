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