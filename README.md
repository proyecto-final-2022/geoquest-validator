# GeoQuest Validator

### Descripción:

API REST para la gestión y validación de cupónes.


### Desarrollo:

#### Requerimientos:
Para el desarrollo es necesario tener instalada la herramienta [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

Para el ultimo comando es necesario tener instalada la herramienta *ghcid*.
Para instalarla hay que ejecutar el siguiente comando por unica vez:
- También es util tener instalado _make_ como shortcut para el ultimo comando.
```
stack install ghcid
```

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
ghcid
```
- Si no tienen instalado _make_ pueden usar el comando completo:
    ```
    ghcid --test ":main"
    ```
- Muy util para el desarrollo, levanta el servidor (corre `Main.hs`) y automaticamente se reinicia cuando hacemos algun cambio.
- No es necesario compilar ni ejecutar. Solo hay que dejarlo corriendo por atrás.
