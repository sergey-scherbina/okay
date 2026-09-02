#!/bin/sh
# Provision the dockerized Postgres (container okay-pg) for the mTLS
# live test (TestPgMtls): a client CA, a client certificate whose CN
# is the role `okay_mtls`, the role itself, and a pg_hba rule that
# demands the certificate for that role ONLY — password roles and
# plaintext connections stay as they were. Idempotent.
set -eu
C=${1:-okay-pg}
D=/var/lib/postgresql/data

docker exec -u postgres "$C" sh -eu -c "
cd $D
if [ ! -f okay_mtls.crt ]; then
  openssl req -new -x509 -days 3650 -nodes -subj '/CN=okay-client-ca' \
    -keyout okay_mtls_ca.key -out okay_mtls_ca.crt 2>/dev/null
  openssl req -new -nodes -subj '/CN=okay_mtls' \
    -keyout okay_mtls.key -out okay_mtls.csr 2>/dev/null
  openssl x509 -req -in okay_mtls.csr -CA okay_mtls_ca.crt -CAkey okay_mtls_ca.key \
    -CAcreateserial -days 3650 -out okay_mtls.crt 2>/dev/null
  chmod 0600 okay_mtls.key okay_mtls_ca.key
fi
grep -q 'okay_mtls' pg_hba.conf || \
  sed -i 's|^host all all all scram-sha-256|hostssl all okay_mtls all cert clientcert=verify-full\nhost all all all scram-sha-256|' pg_hba.conf
"
# each -c is its own transaction: ALTER SYSTEM refuses to share one.
docker exec -u postgres "$C" psql -U okay -d okay -v ON_ERROR_STOP=1 -q \
  -c "alter system set ssl_ca_file = 'okay_mtls_ca.crt'" \
  -c "do \$\$ begin
        if not exists (select 1 from pg_roles where rolname = 'okay_mtls') then
          create role okay_mtls login;
        end if;
      end \$\$" \
  -c "grant connect on database okay to okay_mtls" \
  -c "select pg_reload_conf()" >/dev/null
echo "okay-pg: mTLS provisioned (role okay_mtls, ca okay_mtls_ca.crt, hostssl cert rule)"
