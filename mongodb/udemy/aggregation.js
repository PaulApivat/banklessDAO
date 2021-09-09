db.bounties
  .aggregate([
    { $match: { season: 1 } },
    {
      $group: { _id: { reward: "$reward.currency" }, totalAmount: { $sum: 1 } },
    },
  ])
  .pretty();

// [
//    { _id: { reward: 'BANK' }, totalAmount: 16 },
//    { _id: { reward: 'bank' }, totalAmount: 5 }
//  ]

db.bounties
  .aggregate([
    { $match: { season: 1 } },
    { $group: { _id: { reward: "$reward.amount" }, totalAmount: { $sum: 1 } } },
  ])
  .pretty();

// match season 1
// group by $reward.amount
// we can see two bounties went for exactly 10 BANK, two bounties for 50 BANK, three bounties for 2500
//
[
  { _id: { reward: 10 }, totalAmount: 2 },
  { _id: { reward: 1000 }, totalAmount: 4 },
  { _id: { reward: 750 }, totalAmount: 1 },
  { _id: { reward: 500 }, totalAmount: 1 },
  { _id: { reward: 300 }, totalAmount: 1 },
  { _id: { reward: 50 }, totalAmount: 2 },
  { _id: { reward: 2500 }, totalAmount: 3 },
  { _id: { reward: 1 }, totalAmount: 1 },
  { _id: { reward: 10000 }, totalAmount: 1 },
  { _id: { reward: 4000 }, totalAmount: 1 },
  { _id: { reward: 5000 }, totalAmount: 1 },
  { _id: { reward: 1500 }, totalAmount: 3 },
];

// sorting introduced
// sorting done on the output of $group
// Same operations as above, but sort by totalAmount (descending order)

db.bounties
  .aggregate([
    { $match: { season: 1 } },
    { $group: { _id: { reward: "$reward.amount" }, totalAmount: { $sum: 1 } } },
    { $sort: { totalAmount: -1 } },
  ])
  .pretty();

[
  { _id: { reward: 1000 }, totalAmount: 4 },
  { _id: { reward: 2500 }, totalAmount: 3 },
  { _id: { reward: 1500 }, totalAmount: 3 },
  { _id: { reward: 10 }, totalAmount: 2 },
  { _id: { reward: 50 }, totalAmount: 2 },
  { _id: { reward: 750 }, totalAmount: 1 },
  { _id: { reward: 10000 }, totalAmount: 1 },
  { _id: { reward: 300 }, totalAmount: 1 },
  { _id: { reward: 5000 }, totalAmount: 1 },
  { _id: { reward: 1 }, totalAmount: 1 },
  { _id: { reward: 4000 }, totalAmount: 1 },
  { _id: { reward: 500 }, totalAmount: 1 },
];

db.bounties
  .aggregate([
    { $match: { season: 1 } },
    { $group: { _id: { reward: "$reward.amount" }, totalAmount: { $avg: 1 } } },
  ])
  .pretty();

// How much BANK was allocated for bounties in Season 1?

db.bounties
  .aggregate([
    { $match: { season: 1 } },
    {
      $group: {
        _id: { reward: "$reward.currency" },
        total: { $sum: "$reward.amount" },
      },
    },
  ])
  .pretty();

[
  { _id: { reward: "BANK" }, total: 25570 },
  { _id: { reward: "bank" }, total: 11101 },
];

// Who created the most bounties? (in Season 1), in descending order

db.bounties
  .aggregate([
    { $match: { season: 1 } },
    {
      $group: {
        _id: { createdBy: "$createdBy.discordHandle" },
        totalAmount: { $sum: 1 },
      },
    },
    { $sort: { totalAmount: -1 } },
  ])
  .pretty();

// Who claimed the most bounties (in Season 1), in descending order

db.bounties
  .aggregate([
    { $match: { season: 1 } },
    {
      $group: {
        _id: { claimedBy: "$claimedBy.discordHandle" },
        totalAmount: { $sum: 1 },
      },
    },
    { $sort: { totalAmount: -1 } },
  ])
  .pretty();
