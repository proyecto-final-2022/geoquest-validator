# GeoQuest Validator

[![Haskell CI](https://github.com/proyecto-final-2022/geoquest-validator/actions/workflows/haskell.yml/badge.svg)](https://github.com/proyecto-final-2022/geoquest-validator/actions/workflows/haskell.yml)

### Descripción:

API REST para la gestión y validación de cupónes.


### Desarrollo:

#### Requerimientos:
Para el desarrollo es necesario tener instalada la herramienta [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

Para el ultimo comando es necesario tener instalada la herramienta *ghcid*.
Para instalarla hay que ejecutar el siguiente comando por unica vez:
```
stack install ghcid
```
- Tambien es util tener instalado el comando _make_ para no tener escribir el ultimo comando completo.


#### Comandos utiles:

```
stack build
```
- Compila el codigo a un ejecutable "geoquest-validator-exe".

```
stack exec geoquest-validator-exe
```
- Ejecuta el ejecutable obtenido de `stack build`.

```
stack run
```
- Compila y ejecuta el ejecutable generado.
- Es lo mismo que hacer `stack build && stack exec geoquest-validator-exe`.

```
make dev
```
- Si no tienen instalado el comando _make_ pueden usar el comando completo:
    ```
    ghcid -r --target=geoquest-validator:geoquest-validator-exe
    ```
- Muy util para el desarrollo, levanta el servidor y se reinicia automaticamente cuando se hace una modificacion en el codigo.
- No es necesario compilar ni ejecutar, el codigo se ejecuta en modo "interpretable".
