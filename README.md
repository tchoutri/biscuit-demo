# Biscuit demo

This repository is a showcase for the [Biscuit](https://biscuitsec.org) authorization token system.

Three services are contained in this project:

* The token dispenser, that will send a token to the client based on email;
* The API server, that will proces requests from the client

## Demo

Start the services:

```bash
$ cabal run dispenser

[+] Starting the Token dispenser on http://localhost:8900

$ cabal run api-server

[+] Starting the API server on http://localhost:8902
```

Then get the token from the dispenser with this command (using [httpie](https://httpie.io)):

```bash
$ http POST http://localhost:8900/tokens email=admin@example.org              
HTTP/1.1 200 OK
Content-Type: application/json;charset=utf-8
Date: Fri, 11 Mar 2022 10:06:22 GMT
Server: Warp/3.3.20
Transfer-Encoding: chunked

{
    "token": "EqECCrYBCiQ1ZGQ5OGIzNy0wMWRmLTQ0YWQtOGEzYi0yZDg2YjU4MDUzYjEKJGFiNTNlN2ViLTdmZjItNDM4ZC1iNGRiLTBjZDEwMTNkOWE2OAoDYXBpCgRyZWFkCgdzZXJ2aWNlCgp1c2VyX2dyb3VwCgd1c2VyX2lkCgV3cml0ZRgCIggKBggNEgIYCCIQCg4IBBICGAoSAhgMEgIYByIQCg4IBBICGA4SAhgMEgIYByIICgYICxICGAkSJAgAEiC-S8ZXcwjZK0AVM3hFHkdWGr1x1WKa57rM76ERm84m0hpAm79G03LibmVHX9UOAW4g12i6XfU3dwmSge4Xn1cNM7z-3d2TkT4C_oBipJE-L_d4CgaUjAb17Qm3pe22L9NmDyIiCiCzUfLfQJGOlrLJ0NOM8eW3EUG7Ul4EhciUFeLes38MuA=="
}
```

put this token in an environment variable, aptly named `$biscuit`:

```bash

$ export biscuit="EqECCrYBCiQ1ZGQ5OGIzNy0wMWRmLTQ0YWQtOGEzYi0yZDg2YjU4MDUzYjEKJGFiNTNlN2ViLTdmZjItNDM4ZC1iNGRiLTBjZDEwMTNkOWE2OAoDYXBpCgRyZWFkCgdzZXJ2aWNlCgp1c2VyX2dyb3VwCgd1c2VyX2lkCgV3cml0ZRgCIggKBggNEgIYCCIQCg4IBBICGAoSAhgMEgIYByIQCg4IBBICGA4SAhgMEgIYByIICgYICxICGAkSJAgAEiC-S8ZXcwjZK0AVM3hFHkdWGr1x1WKa57rM76ERm84m0hpAm79G03LibmVHX9UOAW4g12i6XfU3dwmSge4Xn1cNM7z-3d2TkT4C_oBipJE-L_d4CgaUjAb17Qm3pe22L9NmDyIiCiCzUfLfQJGOlrLJ0NOM8eW3EUG7Ul4EhciUFeLes38MuA=="
```

and now let's query the API server:

```bash
$ http GET "http://localhost:8902/user_groups/5dd98b37-01df-44ad-8a3b-2d86b58053b1" "Authorization:Bearer $biscuit"
HTTP/1.1 200 OK
Content-Type: application/json;charset=utf-8
Date: Fri, 11 Mar 2022 10:08:36 GMT
Server: Warp/3.3.20
Transfer-Encoding: chunked

{
    "name": "Passing the time",
    "userGroupId": "5dd98b37-01df-44ad-8a3b-2d86b58053b1"
}
```

We can see that the biscuit was validated and the checks have passed.
