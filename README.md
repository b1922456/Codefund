# 🎯 Codefund - Open Source Bounty System

> 💰 Fund fixes and features transparently on the blockchain

## 🚀 Overview

Codefund is a decentralized bounty system built on Stacks that enables transparent funding of open source development. Create bounties for bug fixes, new features, or improvements, and let developers compete to deliver solutions.

## ✨ Features

- 🎯 **Create Bounties** - Post development tasks with STX rewards
- 💸 **Crowdfund Projects** - Multiple funders can contribute to bounties  
- 👥 **Assign Developers** - Bounty creators can assign work to specific developers
- 📝 **Submit Work** - Developers submit their completed work for review
- ✅ **Approve & Pay** - Automatic payment upon work approval
- 🔄 **Bounty Management** - Cancel, reject, or reassign bounties as needed
- 💼 **Platform Fees** - Sustainable 2.5% platform fee model

## 🛠 Usage

### Creating a Bounty

```clarity
(contract-call? .codefund create-bounty 
  "Fix login bug" 
  "Users cannot login with special characters in password"
  u1000  ;; deadline block height
  u100   ;; initial reward in microSTX
)
```

### Funding an Existing Bounty

```clarity
(contract-call? .codefund fund-bounty u1 u50) ;; Add 50 microSTX to bounty #1
```

### Assigning Work

```clarity
(contract-call? .codefund assign-bounty u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

### Submitting Work

```clarity
(contract-call? .codefund submit-work u1 "https://github.com/user/repo/pull/123")
```

### Approving Submission

```clarity
(contract-call? .codefund approve-submission u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

## 📊 Bounty Lifecycle

1. **🆕 Open** - Bounty created, accepting funds and applications
2. **👤 Assigned** - Developer assigned to work on the bounty  
3. **📤 Submitted** - Work submitted for review
4. **✅ Completed** - Work approved and payment sent
5. **❌ Cancelled** - Bounty cancelled, funds returned

## 🔍 Read-Only Functions

- `get-bounty` - Get bounty details
- `get-bounty-funds` - Get total funding for a bounty
- `get-user-funding` - Get user's contribution to a bounty
- `get-submission` - Get submission details
- `get-contract-balance` - Get total contract balance

## 💡 Getting Started

1. Deploy the contract to Stacks testnet/mainnet
2. Create your first bounty with `create-bounty`
3. Share the bounty ID with potential contributors
4. Manage submissions and approve completed work

## 🏗 Development

Built with:
- **Clarity** - Smart contract language for Stacks
- **Clarinet** - Development environment and testing framework

## 📈 Platform Economics

- Platform fee: 2.5% of bounty rewards
- Fees collected support platform development and maintenance
- Transparent fee structure with no hidden costs

## 🤝 Contributing

This is an open source project! Feel free to:
- Report bugs 🐛
- Suggest features 💡  
- Submit pull requests 🔄
- Fund bounties for improvements 💰

---

