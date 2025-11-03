# Installing SQLite Support for CNS

CNS v1.3.0 includes database operations using SQLite.

## Installation

### Linux (Debian/Ubuntu)
```bash
sudo apt-get update
sudo apt-get install sqlite3 libsqlite3-dev
```

### macOS
```bash
brew install sqlite3
```

### Verify Installation
```bash
sqlite3 --version
```

## Usage in CNS

Once installed, you can use database operations in CNS:

```cns
Effect: DB CONNECT TO "myapp.db" AS db
Effect: DB EXECUTE "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)" AS db
Effect: DB EXECUTE "INSERT INTO users (name) VALUES ('Alice')" AS db
Effect: DB QUERY "SELECT * FROM users" AS db INTO rows
```

## Without SQLite

If SQLite is not installed, CNS will print a warning and database operations will be no-ops (no errors, just skipped).

This allows CNS programs to run in environments without database support, though database functionality will be unavailable.
