INSERT INTO nxo_api_keys(user_id, api_key)
VALUES ($1, $2)
ON CONFLICT (user_id) DO UPDATE SET api_key = $2;
