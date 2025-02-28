# Data Storage and Access Control Smart Contract

## Overview
This smart contract provides a secure and decentralized system for storing and managing health data. It allows users to store encrypted health records, grant or revoke access to specific individuals, and retrieve data based on permissions. The contract ensures that only authorized users can access or modify health records.

## Features
- **Decentralized Health Data Storage**: Users can store a reference to their health data (such as an IPFS hash) securely on the blockchain.
- **Access Control Mechanism**: Users can grant read and write permissions to other principals for a specified duration.
- **Authorization Checks**: Ensures that only authorized users can access or modify data.
- **Data Integrity**: Health data records are stored with timestamps to track modifications.

## Constants
- `contract-owner`: The original deployer of the contract.
- `err-owner-only (u100)`: Error for operations restricted to the contract owner.
- `err-unauthorized (u101)`: Error when an unauthorized user attempts an operation.
- `err-already-exists (u102)`: Error when trying to create a record that already exists.
- `err-not-found (u103)`: Error when attempting to access a non-existent record.

## Data Structures
### Data Variables
- `health-records`: A mapping of principal addresses to their respective health data records.
  - `data-hash`: A 64-character UTF-8 string representing the IPFS hash or encrypted data reference.
  - `is-active`: Boolean flag to indicate if the record is active.
  - `last-updated`: A timestamp representing the last modification.

- `access-permissions`: A mapping of owner-accessor pairs to access permission details.
  - `can-read`: Boolean indicating if the accessor can read the data.
  - `can-write`: Boolean indicating if the accessor can modify the data.
  - `expiration`: A timestamp representing when access expires.

## Functions
### Private Functions
#### `is-authorized(owner principal, accessor principal) -> bool`
Checks whether the accessor has valid read permissions for the owner's data.

### Public Functions
#### `store-health-data(data-hash (string-utf8 64)) -> (ok unit)`
Stores or updates the health data for the transaction sender.

#### `grant-access(accessor principal, can-read bool, can-write bool, duration uint) -> (ok unit)`
Grants access to a specified principal with permissions for a given duration.

#### `revoke-access(accessor principal) -> (ok unit)`
Revokes access from a specific principal.

#### `read-health-data(owner principal) -> (ok {data-hash, is-active, last-updated}) | (err u103 | u101)`
Retrieves health data if the caller is the owner or has been granted read access.

#### `delete-health-data() -> (ok unit)`
Deletes the transaction sender's health data record.

### Read-Only Functions
#### `check-access(owner principal, accessor principal) -> bool`
Returns whether the accessor currently has read access to the owner's data.

#### `get-access-details(owner principal, accessor principal) -> (option {can-read, can-write, expiration})`
Retrieves the access details for a specified owner-accessor pair.

## Security Considerations
- **Access control**: Only authorized users can read or modify health data.
- **Expiration-based access**: Access permissions are time-bound to enhance security.
- **Immutable data reference**: Data storage uses hashes, ensuring integrity and security.

## Usage
1. **Store health data**: Users upload their encrypted data to IPFS and store the reference.
2. **Grant access**: Users provide access to other principals with time-limited permissions.
3. **Read data**: Authorized users can retrieve the stored data reference.
4. **Manage permissions**: Users can revoke or modify access as needed.

## License
This contract is open-source and can be used or modified under the terms of an appropriate open-source license.

