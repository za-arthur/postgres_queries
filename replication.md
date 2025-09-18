# Replication lag

```sql
select application_name,
    pg_size_pretty(pg_wal_lsn_diff(pg_current_wal_lsn(), sent_lsn)) send_lag,
    pg_size_pretty(pg_wal_lsn_diff(sent_lsn, replay_lsn)) replay_lag,
    replay_lag
from pg_stat_replication;
```

```sql
select slot_name, slot_type, active,
    case when not pg_is_in_recovery() then pg_size_pretty(pg_current_wal_lsn() - restart_lsn) end as current_lag_bytes,
    case when not pg_is_in_recovery() then pg_size_pretty(pg_current_wal_lsn() - confirmed_flush_lsn) end as current_flush_lag_bytes
from pg_replication_slots s
order by s.slot_name;
```