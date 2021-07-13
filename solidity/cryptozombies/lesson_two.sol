pragma solidity >=0.5.0 <0.6.0;

import "./zombiefactory.sol";

// KittyInterface
// read CryptoKitties data
contract KittyInterface {
    function getKitty(uint256 _id) external view returns (
    bool isGestating,
    bool isReady,
    uint256 cooldownIndex,
    uint256 nextActionAt,
    uint256 siringWithId,
    uint256 birthTime,
    uint256 matronId,
    uint256 sireId,
    uint256 generation,
    uint256 genes
    );
}

// ZombieFeeding inherits from ZombieFactory
contract ZombieFeeding is ZombieFactory {

    address ckAddress = 0x06012c8cf97BEaD5deAe237070F9587f8E7A266d;
    // initialize kittyContract using ckAddress
    KittyInterface kittyContract = KittyInterface(ckAddress);


    // using storage vs. memory
    // ensure that only person who owns the zombie can feed it
    function feedAndMultiply(uint _zombieId, uint _targetDna) public {
      require(msg.sender == zombieToOwner[_zombieId]);
      Zombie storage myZombie = zombies[_zombieId];
      // formula for calculating new zombie's DNA
      // inherit _createZombie from zombiefactory
      _targetDna = _targetDna % dnaModulus;
      uint newDna = (myZombie.dna + _targetDna) / 2;
      _createZombie("NoName", newDna);
  }
}