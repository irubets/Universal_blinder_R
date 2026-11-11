# Cross-language fixture (SYNTHETIC)

* `source/` synthetic SDTM/ADaM datasets (xpt)
* `maps/` a key built with a FIXED seed -- test use only, never for a real study
* `expected_blinded/` Python reference output (dates as SAS numbers) + labels/formats
* `blinding_config.json` the config used

Rebuild with `python -m universal_blinder.fixture <dir>`.
