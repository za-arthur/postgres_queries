# Check role membership

```sql
SELECT
    r.rolname       AS member_role,
    m.rolname       AS target_role,
    am.admin_option AS has_admin_option
FROM pg_auth_members am
JOIN pg_roles r ON r.oid = am.member
JOIN pg_roles m ON m.oid = am.roleid
WHERE r.rolname = 'postgres';
```

```sql
WITH RECURSIVE x AS
(
  SELECT member::regrole,
         roleid::regrole AS role,
         member::regrole || ' -> ' || roleid::regrole AS path
  FROM pg_auth_members AS m
  UNION ALL
  SELECT x.member::regrole,
         m.roleid::regrole,
         x.path || ' -> ' || m.roleid::regrole
  FROM pg_auth_members AS m
    JOIN x ON m.member = x.role
  )
SELECT member, role, path
FROM x
ORDER BY member::text, role::text;
```
