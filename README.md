# Dynamic Multisig Treasury


## About

This project implements a Treasury whose authorized signatories can be changed dynamically.  It was developed as my *final project* for the **Cardano Developer Professional Program** at Emurgo Academy.

-   *Author:*  Antonio Hernández-Garduño

-   *Date (first version)*:  January 2023

-   *Plutus apps revision:*  v1.0.0

-   *Plutus version:*  V2


## Description

A dynamic multisig Treasury is implemented as a pair of script addresses, which we will refer as the *treasury* and the *registrar*.  The treasury locks the Treasury funds and the registrar keeps the list of authorized signatories and a ratio `n:m` of integer numbers which correspond to a "n out of m" voting threshold needed for operation approval.

The data and state of both the treasury and the registrar are stored in UTxO's at their respective script addresses and each is identified via the presence of an NFT (*thread token*).


### Treasury

The procedure for releasing a payment from the treasury involves the following steps:

-   An authorized signatory makes a *payment proposal*, which includes the beneficiary address, the amount and a deadline for votes to be collected.
-   Other authorized signatories vote for approval of the payment proposal.  If the ratio of votes to total number of registered signatories is above the required threshold, then payment is released.

Currently the treasury can only handle one payment proposal at a time.  If a payment proposal is active, then the treasury's datum (more precisely, the datum at the UTxO storing the NFT representing the treasury) is  `Just Payment` , where  `Payment`  is a datatype storing the beneficiary, amount and deadline.  If no payment proposal is active, then the datum is  `Nothing` .

There are two redeemers associated with actions on the treasury:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Redeemer</th>
<th scope="col" class="org-left">Action</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">ProposePay amount recipient deadline</td>
<td class="org-left">Opens a payment proposal</td>
</tr>


<tr>
<td class="org-left">SignPay</td>
<td class="org-left">Votes in favor of proposal</td>
</tr>
</tbody>
</table>

Only registered signatories can perform such actions.

The treasury's validator uses CIP 31 "reference inputs", enabled in the Vasil HFC upgrade, for accessing the data stored at the registrar.


### Registrar

The registrar's datum (again, at the UTxO with the corresponding registrar's NFT) stores the current list of signatories, the threshold `n:m` ratio (represented by a pair of integers `(n, m)`), and the registrar's "state".  The state can be either  `StandBy` ,  `Adding ps vs`  or  `Removing ps vs` , depending on whether there is no active proposal for changing the list of signatories, or a proposal for either adding or removing signatories is active.  Here  `ps`  and  `vs`  stand for, respectively, the proposed signatories to be added or removed and the accumulated votes in favor of such change.  A deadline for the voting process to be completed is also stored in the registrar's datum.

The redeemers associated with actions on the registrar are:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Redeemer</th>
<th scope="col" class="org-left">Action</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">ProposeAdd signatories</td>
<td class="org-left">Proposes the addition of new signatories</td>
</tr>


<tr>
<td class="org-left">ProposeRemove signatories</td>
<td class="org-left">Proposes deletion of some signatories</td>
</tr>


<tr>
<td class="org-left">ApproveAdd</td>
<td class="org-left">Votes in favor of proposed addition</td>
</tr>


<tr>
<td class="org-left">ApproveRemove</td>
<td class="org-left">Votes in favor of proposed deletion</td>
</tr>


<tr>
<td class="org-left">Reset</td>
<td class="org-left">Cleans state if deadline was reached</td>
</tr>
</tbody>
</table>

Only registered signatories can perform such actions, except for `Reset` which can be triggered by anyone, requiring only that the proposal's deadline has expired.


## Files

The dynamic treasury so far described is implemented in the following files inside directory `src`:

-   `Validators.hs` :  On-chain code with the script validators associated with the Registrar and the Treasury.
-   `Setup.hs` :  Off-chain code for initialization, as well as the overall schema.
-   `Registrar.hs` :  Off-chain code associated with actions performed on the registrar.
-   `Treasury.hs` :  Off-chain code associated with actions performed on the treasury.
-   `Params.hs` :  Global parameters.
-   `Trace.hs` :  Emulator trace code describing various scenarios.
    
    Both the on-chain and off-chain code is written in *Plutus* (thus, in Haskell).


## Scenarios and tests

Testing of some scenarios is done using the *Emulator Trace*.  Please refer to file `Trace.hs` which contains and documents various scenarios.

Use the command `test` to run the trace emulator in the REPL.

