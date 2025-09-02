# 🏆 Hackbounty - Hackathon Rewards Contract

A Clarity smart contract for managing hackathon events with automatic prize distribution and project-based payouts.

## 🚀 Features

- 🎯 **Event Creation**: Organizers can create hackathon events with bounty pools
- 📝 **Project Registration**: Participants can register their projects during open periods
- 👨‍⚖️ **Judge Management**: Authorized judges can evaluate and score projects
- 🏅 **Automatic Rankings**: Smart prize distribution based on final rankings
- 💰 **Instant Payouts**: Winners can claim prizes immediately after results
- ⚡ **Emergency Controls**: Safety mechanisms for organizers

## 📋 Contract Functions

### 🎪 Event Management

#### `create-event`
Creates a new hackathon event with specified parameters.
```clarity
(create-event "My Hackathon" "Description" u1000000 u1000 u500)
```
- **name**: Event name (max 64 chars)
- **description**: Event description (max 256 chars) 
- **total-bounty**: Prize pool in microSTX
- **registration-duration**: Registration period in blocks
- **judging-duration**: Judging period in blocks

#### `add-judge`
Authorizes judges for an event (organizer only).
```clarity
(add-judge u1 'SP1JUDGE...)
```

### 🛠️ Project Management

#### `register-project`
Registers a project for a hackathon event.
```clarity
(register-project u1 "My Project" "Cool DApp" "github.com/user/repo" (some "demo.com"))
```
- **event-id**: Target hackathon ID
- **name**: Project name
- **description**: Project description
- **repository-url**: GitHub repository
- **demo-url**: Optional demo link

### ⚖️ Judging Process

#### `vote-project`
Judges can score projects (0-100 points).
```clarity
(vote-project u1 u85)
```

#### `finalize-rankings`
Organizer finalizes winners and rankings.
```clarity
(finalize-rankings u1 (list {project-id: u1, rank: u1} {project-id: u2, rank: u2}))
```

### 💸 Prize Distribution

#### `claim-prize`
Winners claim their prize money.
```clarity
(claim-prize u1)
```

Prize distribution:
- 🥇 **1st Place**: 50% of total bounty
- 🥈 **2nd Place**: 30% of total bounty  
- 🥉 **3rd Place**: 20% of total bounty

### 🆘 Emergency Functions

#### `emergency-withdraw`
Organizer can withdraw funds after extended period (safety mechanism).
```clarity
(emergency-withdraw u1)
```

## 📖 Read-Only Functions

- `get-event`: Get event details
- `get-project`: Get project information
- `get-participant-project`: Check participant registration
- `get-judge-status`: Verify judge authorization
- `get-ranking`: View final rankings
- `get-contract-balance`: Check contract STX balance

## 🔄 Workflow

1. **🎪 Setup Phase**
   - Organizer creates event with `create-event`
   - Organizer adds judges with `add-judge`

2. **📝 Registration Phase**
   - Participants register projects with `register-project`
   - Registration window defined by event parameters

3. **⚖️ Judging Phase**
   - Judges evaluate projects with `vote-project`
   - Scoring period follows registration closure

4. **🏆 Results Phase**
   - Organizer finalizes rankings with `finalize-rankings`
   - Winners automatically determined by scores

5. **💰 Payout Phase**
   - Winners claim prizes with `claim-prize`
   - Instant STX transfers to winner wallets

## 🛡️ Security Features

- **Time-based Access Control**: Functions locked to specific phases
- **Authorization Checks**: Role-based permissions for organizers/judges
- **Double-spending Prevention**: Projects can't be registered twice
- **Emergency Withdrawal**: Safety valve for stuck funds
- **Input Validation**: Parameter bounds checking

## 🚦 Error Codes

- `u100`: Unauthorized access
- `u101`: Resource not found
- `u102`: Already exists
- `u103`: Invalid amount
- `u104`: Event not active
- `u105`: Insufficient funds
- `u106`: Registration closed
- `u107`: Judging not started
- `u108`: Already judged
- `u109`: Not participant
- `u110`: Invalid rank

## 🧪 Testing

Run the test suite:
```bash
clarinet test
```

## 📄 License

MIT License - Build awesome hackathons! 🚀
