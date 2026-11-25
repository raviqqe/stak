# Agent guidelines

## Testing

- Do not run all the integration tests of the `stak-tools` interpreter locally.
  - i.e. `tools/integration_test.sh -i stak-tools`
  - They take very long to finish on local machines. Leave them to CI.
  - If they are necessary, run only specific feature files.
