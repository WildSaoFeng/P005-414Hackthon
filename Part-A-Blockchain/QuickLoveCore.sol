pragma solidity ^0.4.11;
/*
 * Written by ClaudeZsb, 04/19/2018.
 * Idea comes from DoraHackthon.
 *
 *
 * QuickLoveCore presently only realizes the principle funtions. It is composed of FlowerCore, Expressionwall and MariageInsurance while all of them are very heavy.  
 * To deploy QuickLoveCore, about 6000000 gas is needed. So At present, it MUST NOT be deployed in main Ethereum network. 
 * Plan of improvement: contract that manages insurance service shall be deployed as a seperate contract from main, just like the auction contract.
 * Change some important functions(insurance fund related) to be ONLY called by main contract. Those functions could be managed by one part of the main contract.
 * That makes the main contract to manage all funds, while insurance contract manages only the insurance data.
*/

contract Expressionwall {
	event CreateExpress(address, string, uint256);
	mapping (uint256 => address) expressionIndexToOwner;
	//mapping (address => uint256) public ownershipExpressionCount;
	mapping (address => uint256[]) ownershipExpressionIdList;
	
	struct Expression {
		uint64 time;
		string data;
	}
	
	Expression[] expressiones;
	uint256 public expressionCount;
	
	function createExpress(string _data) external returns(uint) {
		Expression memory _expression = Expression({
			time: uint64(now),
			data: _data
		});
		uint256 expressionId = expressiones.push(_expression) - 1;
		require(expressionId == uint256(uint32(expressionId)));
		expressionCount++;
		expressionIndexToOwner[expressionId] = msg.sender;
		//ownershipExpressionCount[msg.sender] ++;
		ownershipExpressionIdList[msg.sender].push(expressionId);
		CreateExpress(msg.sender, _data, expressionId);
		return expressionId;
	}
	
	function getExpressionByIndex(uint256 _index) external view returns(uint64 time, string data) {
	    require(_index < expressionCount);
		time = expressiones[_index].time;
		data = expressiones[_index].data;
	}
	
	function getMyExpressionByIndex(uint256 _index) external view returns(uint64 time, string data) {
	    uint256[] storage indexList = ownershipExpressionIdList[msg.sender];
	    require(_index < indexList.length);
		time = expressiones[indexList[_index]].time;
		data = expressiones[indexList[_index]].data;
	}
	
	function getMyExpressionCount() external view returns(uint256) {
	    return ownershipExpressionIdList[msg.sender].length;
	}
	
	//struct can not be used in external function return
	//It will cost much for getting all expressiones. It's better to use the getExpressionByIndex function to get several recent experssiones.
	/*
	function getAllExpression() external returns(uint256 count, uint64[] times, string[] expressiones) {
		count = expressiones.length;
		for (uint i = 0; i< count; i++) {
			times.push(expressiones[i].time);
			expressions.push(expressiones[i].data);
		}
	}
	*/
}


contract Ownable {
  address public owner;

  function Ownable() {
    owner = msg.sender;
  }

  modifier onlyOwner() {
    require(msg.sender == owner);
    _;
  }

  function transferOwnership(address newOwner) onlyOwner {
    if (newOwner != address(0)) {
      owner = newOwner;
    }
  }
}

contract Pausable is Ownable {
  event Pause();
  event Unpause();

  bool public paused = false;


  /**
   * @dev modifier to allow actions only when the contract IS paused
   */
  modifier whenNotPaused() {
    require(!paused);
    _;
  }

  /**
   * @dev modifier to allow actions only when the contract IS NOT paused
   */
  modifier whenPaused {
    require(paused);
    _;
  }

  /**
   * @dev called by the owner to pause, triggers stopped state
   */
  function pause() onlyOwner whenNotPaused returns (bool) {
    paused = true;
    Pause();
    return true;
  }

  /**
   * @dev called by the owner to unpause, returns to normal state
   */
  function unpause() onlyOwner whenPaused returns (bool) {
    paused = false;
    Unpause();
    return true;
  }
}


contract FlowerBase is Pausable {
    event Sale(address indexed to, uint256 flowerId);
    event Transfer(address indexed from, address indexed to, uint256 flowerId);
    
    //At present, a flower differs from others only in its type and color. If needed, to add more mermber in this struct
    //and complete relevant functions.
    struct Flower {
        uint32 flowerType;
        uint32 flowerColor;
    }
    
    Flower[] flowers;
    uint32 typeCount = 10;
    uint32 colorCount = 8;
    
    function changeFlowerType(uint32 newTypeCount) external onlyOwner whenNotPaused{
        require(newTypeCount > 0);
        typeCount = newTypeCount;
    }
    
    SaleClockAuction public saleAuction;
    
    mapping(uint256 => address) flowerIdToOwner;
    mapping(address => uint256) personalFlowerCount;
    mapping(uint256 => address) public flowerIdToApproved;
    
    function _transfer(address _from, address _to, uint256 _index) internal {
        personalFlowerCount[_to]++;
        flowerIdToOwner[_index] = _to;
        if (_from != address(0)) {
            personalFlowerCount[_from]--;
            delete flowerIdToApproved[_index];
            Transfer(_from, _to, _index);
        } else {
            Sale(_to, _index);
        }
        
    }
    
    function _createFlower(address owner, uint32 _type, uint32 _color) internal returns(uint256){
        //do check work during external call.
        //require((_type < typeCount) && (_color < colorCount));
        Flower memory _newFlower = Flower(_type, _color);
        uint256 flowerId = flowers.push(_newFlower) - 1;
        _transfer(0, owner, flowerId);
        return flowerId;
    }
}

contract ERC721 {
    //function name() constant returns (string name);
    //function symbol() constant returns (string symbol);
    function totalSupply() public view returns (uint256 totalSupply);
    function balanceOf(address _owner) public view returns (uint256 balance);
    function ownerOf(uint256 _tokenId) public view returns (address owner);
    function approve(address _to, uint256 _tokenId) external ;
    function takeOwnership(uint256 _tokenId) external;
    function transfer(address _to, uint256 _tokenId) external;
    //function tokenOfOwnerByIndex(address _owner, uint256 _index) public view returns (uint tokenId); 
    //function tokenMetadata(uint256 _tokenId) view returns (string infoUrl);
    
    event Transfer(address indexed _from, address indexed _to, uint256 _tokenId);
    event Approval(address indexed _owner, address indexed _approved, uint256 _tokenId);
}

contract FlowerOwnership is FlowerBase, ERC721 {
    string public constant name = "CyptoFlower";
    string public constant symbol = "CF";
    
    function totalSupply() public view returns (uint256) {
        return flowers.length;
    }
    
    function balanceOf(address _owner) public view returns (uint256) {
        return personalFlowerCount[_owner];
    }
    
    function ownerOf(uint256 _tokenId) public view returns (address owner) {
        owner = flowerIdToOwner[_tokenId];
        require(owner != address(0));
    }
    
    function _approve(address _to, uint256 _tokenId) internal {
        flowerIdToApproved[_tokenId] = _to;
    }
    
    function approve(address _to, uint256 _tokenId) external {
        require(ownerOf(_tokenId) == msg.sender);
        flowerIdToApproved[_tokenId] = _to;
        Approval(msg.sender, _to, _tokenId);
    }
    
    function transfer(address _to, uint256 _tokenId) external {
        require(_to != address(0));
        require(_to != address(this));
        require(_to != address(saleAuction));
        require(ownerOf(_tokenId) == msg.sender);
        _transfer(msg.sender, _to, _tokenId);
    }
    function takeOwnership(uint256 _tokenId) external {
        require(flowerIdToApproved[_tokenId] == msg.sender);
        _transfer(flowerIdToOwner[_tokenId], msg.sender, _tokenId);
    }
    
    //this function is used only for web3 call.
    function flowersOfOwner(address _owner) external view returns(uint256[]) {
        uint256 balance = balanceOf(_owner);
        if (balance == 0) {
            return new uint256[](0);
        } else {
            uint256[] memory ownerFlowers = new uint256[](balance);
            uint256 index = 0;
            for (uint i = 0; i < totalSupply(); i++) {
                if (ownerOf(i) == _owner) {
                    ownerFlowers[index] = i;
                    index++;
                }
            }
            return ownerFlowers;
        }
    } 
}

contract FlowerSale is FlowerOwnership {
    mapping(uint32 => uint256) public flowersStorage;
    mapping(uint32 => uint256) public flowersPrice;
    //mapping(uint32 => uint256) public flowersDiscount;
    
    uint256 constant public defaultPrice = 1 finney;
    
    event Open();
    event Close();
    event NewDiscount(uint32 _type, uint32 _color, uint8 _size, uint256 _amount);
    
    bool public saleOpened = false;
    
    modifier Opened {
        require(saleOpened == true);
        _;
    }
    
    modifier Closed {
        require(saleOpened == false);
        _;
    }
    
    struct Discount {
        //uint32 flowerType;
        uint32 flowerColor;
        uint8 size;
        uint256 amount;
    }
    
    mapping(uint32 => Discount) public flowersDiscount;
    //Discount[] public discounts;
    
    function openSale() onlyOwner whenNotPaused Closed {
        for(uint32 i = 0; i < typeCount; i++) {
            if(flowersPrice[i] == 0) {
                flowersPrice[i] = defaultPrice;
            }
        }
        saleOpened = true;
        Open();
    }
    
    function closeSale() onlyOwner whenNotPaused Opened {
        saleOpened = false;
        Close();
    }
    
    function changeFlowerPrice(uint32 _type, uint256 _price) external onlyOwner whenNotPaused {
        require(_type < typeCount);
        require(_price != 0);
        flowersPrice[_type] = _price;
    }
    
    function addFlowerStorage(uint32 _type, uint256 _amount) external onlyOwner whenNotPaused {
        require(_type < typeCount);
        require(_amount > 0);
        flowersStorage[_type] += _amount;
    }
    
    function addFlowerDiscount(uint32 _type, uint32 _color, uint8 _size, uint256 _amount) external onlyOwner whenNotPaused {
        require(_type < typeCount);
        require(_size < 100);
        Discount memory _discount = Discount(_color, _size, _amount);
        flowersDiscount[_type] = _discount;
        NewDiscount(_type, _color, _size, _amount);
    }
    
    function _computePrice(uint32 _type, uint32 _color) private view returns(uint256) {
        uint256 initialPrice = flowersPrice[_type];
        Discount storage _discount = flowersDiscount[_type];
        if(_discount.amount == 0) {
            return initialPrice;
        }
        if((_color != _discount.flowerColor) && (_discount.flowerColor < colorCount)) {
            return initialPrice;
        }
        return initialPrice * _discount.size / 100;
    }
    
    function buy(uint32 _type, uint32 _color) external payable whenNotPaused Opened {
        require(_type < typeCount);
        require(_color < colorCount);
        require(flowersStorage[_type] > 0);
        uint256 _price = _computePrice(_type, _color);
        require(msg.value >= _price);
        flowersStorage[_type]--;
        if(flowersDiscount[_type].amount > 0) {
            flowersDiscount[_type].amount--;
        }
        _createFlower(msg.sender, _type, _color);
        if (msg.value > _price) {
            msg.sender.transfer(msg.value - _price);
        }
    }
}

//Copied from CyptoKitties and edited by Claude.
contract ClockAuctionBase {

    // Represents an auction on an NFT
    struct Auction {
        // Current owner of NFT
        address seller;
        // Price (in wei) at beginning of auction
        uint128 startingPrice;
        // Price (in wei) at end of auction
        uint128 endingPrice;
        // Duration (in seconds) of auction
        uint64 duration;
        // Time when auction started
        // NOTE: 0 if this auction has been concluded
        uint64 startedAt;
    }

    // Reference to contract tracking NFT ownership
    ERC721 public nonFungibleContract;

    // Cut owner takes on each auction, measured in basis points (1/100 of a percent).
    // Values 0-10,000 map to 0%-100%
    uint256 public ownerCut;

    // Map from token ID to their corresponding auction.
    mapping (uint256 => Auction) tokenIdToAuction;

    event AuctionCreated(uint256 tokenId, uint256 startingPrice, uint256 endingPrice, uint256 duration);
    event AuctionSuccessful(uint256 tokenId, uint256 totalPrice, address winner);
    event AuctionCancelled(uint256 tokenId);

    /// @dev Returns true if the claimant owns the token.
    /// @param _claimant - Address claiming to own the token.
    /// @param _tokenId - ID of token whose ownership to verify.
    function _owns(address _claimant, uint256 _tokenId) internal view returns (bool) {
        return (nonFungibleContract.ownerOf(_tokenId) == _claimant);
    }

    /// @dev Escrows the NFT, assigning ownership to this contract.
    /// Throws if the escrow fails.
    /// @param _tokenId - ID of token whose approval to verify.
    function _escrow(uint256 _tokenId) internal {
        // it will throw if transfer fails
        nonFungibleContract.takeOwnership(_tokenId);
    }

    /// @dev Transfers an NFT owned by this contract to another address.
    /// Returns true if the transfer succeeds.
    /// @param _receiver - Address to transfer NFT to.
    /// @param _tokenId - ID of token to transfer.
    function _transfer(address _receiver, uint256 _tokenId) internal {
        // it will throw if transfer fails
        nonFungibleContract.transfer(_receiver, _tokenId);
    }

    /// @dev Adds an auction to the list of open auctions. Also fires the
    ///  AuctionCreated event.
    /// @param _tokenId The ID of the token to be put on auction.
    /// @param _auction Auction to add.
    function _addAuction(uint256 _tokenId, Auction _auction) internal {
        // Require that all auctions have a duration of
        // at least one minute. (Keeps our math from getting hairy!)
        require(_auction.duration >= 1 minutes);

        tokenIdToAuction[_tokenId] = _auction;

        AuctionCreated(
            uint256(_tokenId),
            uint256(_auction.startingPrice),
            uint256(_auction.endingPrice),
            uint256(_auction.duration)
        );
    }

    /// @dev Cancels an auction unconditionally.
    function _cancelAuction(uint256 _tokenId, address _seller) internal {
        _removeAuction(_tokenId);
        _transfer(_seller, _tokenId);
        AuctionCancelled(_tokenId);
    }

    /// @dev Computes the price and transfers winnings.
    /// Does NOT transfer ownership of token.
    function _bid(uint256 _tokenId, uint256 _bidAmount)
        internal
        returns (uint256)
    {
        // Get a reference to the auction struct
        Auction storage auction = tokenIdToAuction[_tokenId];

        // Explicitly check that this auction is currently live.
        // (Because of how Ethereum mappings work, we can't just count
        // on the lookup above failing. An invalid _tokenId will just
        // return an auction object that is all zeros.)
        require(_isOnAuction(auction));

        // Check that the bid is greater than or equal to the current price
        uint256 price = _currentPrice(auction);
        require(_bidAmount >= price);

        // Grab a reference to the seller before the auction struct
        // gets deleted.
        address seller = auction.seller;

        // The bid is good! Remove the auction before sending the fees
        // to the sender so we can't have a reentrancy attack.
        _removeAuction(_tokenId);

        // Transfer proceeds to seller (if there are any!)
        if (price > 0) {
            // Calculate the auctioneer's cut.
            // (NOTE: _computeCut() is guaranteed to return a
            // value <= price, so this subtraction can't go negative.)
            uint256 auctioneerCut = _computeCut(price);
            uint256 sellerProceeds = price - auctioneerCut;

            // NOTE: Doing a transfer() in the middle of a complex
            // method like this is generally discouraged because of
            // reentrancy attacks and DoS attacks if the seller is
            // a contract with an invalid fallback function. We explicitly
            // guard against reentrancy attacks by removing the auction
            // before calling transfer(), and the only thing the seller
            // can DoS is the sale of their own asset! (And if it's an
            // accident, they can call cancelAuction(). )
            seller.transfer(sellerProceeds);
        }

        // Calculate any excess funds included with the bid. If the excess
        // is anything worth worrying about, transfer it back to bidder.
        // NOTE: We checked above that the bid amount is greater than or
        // equal to the price so this cannot underflow.
        uint256 bidExcess = _bidAmount - price;

        // Return the funds. Similar to the previous transfer, this is
        // not susceptible to a re-entry attack because the auction is
        // removed before any transfers occur.
        msg.sender.transfer(bidExcess);

        // Tell the world!
        AuctionSuccessful(_tokenId, price, msg.sender);

        return price;
    }

    /// @dev Removes an auction from the list of open auctions.
    /// @param _tokenId - ID of NFT on auction.
    function _removeAuction(uint256 _tokenId) internal {
        delete tokenIdToAuction[_tokenId];
    }

    /// @dev Returns true if the NFT is on auction.
    /// @param _auction - Auction to check.
    function _isOnAuction(Auction storage _auction) internal view returns (bool) {
        return (_auction.startedAt > 0);
    }

    /// @dev Returns current price of an NFT on auction. Broken into two
    ///  functions (this one, that computes the duration from the auction
    ///  structure, and the other that does the price computation) so we
    ///  can easily test that the price computation works correctly.
    function _currentPrice(Auction storage _auction)
        internal
        view
        returns (uint256)
    {
        uint256 secondsPassed = 0;

        // A bit of insurance against negative values (or wraparound).
        // Probably not necessary (since Ethereum guarnatees that the
        // now variable doesn't ever go backwards).
        if (now > _auction.startedAt) {
            secondsPassed = now - _auction.startedAt;
        }

        return _computeCurrentPrice(
            _auction.startingPrice,
            _auction.endingPrice,
            _auction.duration,
            secondsPassed
        );
    }

    /// @dev Computes the current price of an auction. Factored out
    ///  from _currentPrice so we can run extensive unit tests.
    ///  When testing, make this function public and turn on
    ///  `Current price computation` test suite.
    function _computeCurrentPrice(
        uint256 _startingPrice,
        uint256 _endingPrice,
        uint256 _duration,
        uint256 _secondsPassed
    )
        internal
        pure
        returns (uint256)
    {
        // NOTE: We don't use SafeMath (or similar) in this function because
        //  all of our public functions carefully cap the maximum values for
        //  time (at 64-bits) and currency (at 128-bits). _duration is
        //  also known to be non-zero (see the require() statement in
        //  _addAuction())
        if (_secondsPassed >= _duration) {
            // We've reached the end of the dynamic pricing portion
            // of the auction, just return the end price.
            return _endingPrice;
        } else {
            // Starting price can be higher than ending price (and often is!), so
            // this delta can be negative.
            int256 totalPriceChange = int256(_endingPrice) - int256(_startingPrice);

            // This multiplication can't overflow, _secondsPassed will easily fit within
            // 64-bits, and totalPriceChange will easily fit within 128-bits, their product
            // will always fit within 256-bits.
            int256 currentPriceChange = totalPriceChange * int256(_secondsPassed) / int256(_duration);

            // currentPriceChange can be negative, but if so, will have a magnitude
            // less that _startingPrice. Thus, this result will always end up positive.
            int256 currentPrice = int256(_startingPrice) + currentPriceChange;

            return uint256(currentPrice);
        }
    }

    /// @dev Computes owner's cut of a sale.
    /// @param _price - Sale price of NFT.
    function _computeCut(uint256 _price) internal view returns (uint256) {
        // NOTE: We don't use SafeMath (or similar) in this function because
        //  all of our entry functions carefully cap the maximum values for
        //  currency (at 128-bits), and ownerCut <= 10000 (see the require()
        //  statement in the ClockAuction constructor). The result of this
        //  function is always guaranteed to be <= _price.
        return _price * ownerCut / 10000;
    }
}

contract SaleClockAuction is Pausable, ClockAuctionBase {

    function SaleClockAuction() public {
        paused = true;
        //by default
        ownerCut = 500;
    }
    
    function setCut(uint32 _cut) external onlyOwner whenPaused {
        require(_cut < 10000);
        ownerCut = _cut;
    }
    function setNFTAddress(address _nftAddress) external onlyOwner whenPaused {
        require(_nftAddress != address(0));
        ERC721 candidateContract = ERC721(_nftAddress);
        nonFungibleContract = candidateContract;
    }

    /// @dev Remove all Ether from the contract, which is the owner's cuts
    ///  as well as any Ether sent directly to the contract address.
    ///  Always transfers to the NFT contract, but can be called either by
    ///  the owner or the NFT contract.
    function withdrawBalance() external {
        address nftAddress = address(nonFungibleContract);

        require(
            msg.sender == owner ||
            msg.sender == nftAddress
        );
        // We are using this boolean method to make sure that even if one fails it will still work
        bool res = nftAddress.send(this.balance);
    }

    /// @dev Creates and begins a new auction.
    /// @param _tokenId - ID of token to auction, sender must be owner.
    /// @param _startingPrice - Price of item (in wei) at beginning of auction.
    /// @param _endingPrice - Price of item (in wei) at end of auction.
    /// @param _duration - Length of time to move between starting
    ///  price and ending price (in seconds).
    /// @param _seller - Seller, if not the message sender
    function createAuction(
        uint256 _tokenId,
        uint256 _startingPrice,
        uint256 _endingPrice,
        uint256 _duration,
        address _seller
    )
        external
        whenNotPaused
    {
        // Sanity check that no inputs overflow how many bits we've allocated
        // to store them in the auction struct.
        require(_startingPrice == uint256(uint128(_startingPrice)));
        require(_endingPrice == uint256(uint128(_endingPrice)));
        require(_duration == uint256(uint64(_duration)));

        require(msg.sender == address(nonFungibleContract));
        _escrow(_tokenId);
        Auction memory auction = Auction(
            _seller,
            uint128(_startingPrice),
            uint128(_endingPrice),
            uint64(_duration),
            uint64(now)
        );
        _addAuction(_tokenId, auction);
    }

    /// @dev Bids on an open auction, completing the auction and transferring
    ///  ownership of the NFT if enough Ether is supplied.
    /// @param _tokenId - ID of token to bid on.
    function bid(uint256 _tokenId)
        external
        payable
        whenNotPaused
    {
        // _bid will throw if the bid or funds transfer fails
        _bid(_tokenId, msg.value);
        _transfer(msg.sender, _tokenId);
    }

    /// @dev Cancels an auction that hasn't been won yet.
    ///  Returns the NFT to original owner.
    /// @notice This is a state-modifying function that can
    ///  be called while the contract is paused.
    /// @param _tokenId - ID of token on auction
    function cancelAuction(uint256 _tokenId)
        external
    {
        Auction storage auction = tokenIdToAuction[_tokenId];
        require(_isOnAuction(auction));
        address seller = auction.seller;
        require(msg.sender == seller);
        _cancelAuction(_tokenId, seller);
    }

    /// @dev Cancels an auction when the contract is paused.
    ///  Only the owner may do this, and NFTs are returned to
    ///  the seller. This should only be used in emergencies.
    /// @param _tokenId - ID of the NFT on auction to cancel.
    function cancelAuctionWhenPaused(uint256 _tokenId)
        whenPaused
        onlyOwner
        external
    {
        Auction storage auction = tokenIdToAuction[_tokenId];
        require(_isOnAuction(auction));
        _cancelAuction(_tokenId, auction.seller);
    }

    /// @dev Returns auction info for an NFT on auction.
    /// @param _tokenId - ID of NFT on auction.
    function getAuction(uint256 _tokenId)
        external
        view
        returns
    (
        address seller,
        uint256 startingPrice,
        uint256 endingPrice,
        uint256 duration,
        uint256 startedAt
    ) {
        Auction storage auction = tokenIdToAuction[_tokenId];
        require(_isOnAuction(auction));
        return (
            auction.seller,
            auction.startingPrice,
            auction.endingPrice,
            auction.duration,
            auction.startedAt
        );
    }

    /// @dev Returns the current price of an auction.
    /// @param _tokenId - ID of the token price we are checking.
    function getCurrentPrice(uint256 _tokenId)
        external
        view
        returns (uint256)
    {
        Auction storage auction = tokenIdToAuction[_tokenId];
        require(_isOnAuction(auction));
        return _currentPrice(auction);
    }

}

contract FlowerAuction is FlowerSale {
    
    function setSaleAuctionAddress(address saleAuctionAddress) external onlyOwner {
        require(saleAuctionAddress != address(0));
        SaleClockAuction candidateContract = SaleClockAuction(saleAuctionAddress);
        saleAuction = candidateContract;
    }
    
    function createSaleAuction(
        uint256 _flowerId,
        uint256 _startingPrice,
        uint256 _endingPrice,
        uint256 _duration
    )
        external
        whenNotPaused
    {
        require(ownerOf(_flowerId) == msg.sender);
        _approve(saleAuction, _flowerId);
        saleAuction.createAuction(
            _flowerId,
            _startingPrice,
            _endingPrice,
            _duration,
            msg.sender
        );
    }
    
    function withdrawAuctionBalances() external onlyOwner {
        saleAuction.withdrawBalance();
    }
}

contract FlowerCore is FlowerAuction {
    function getFlower(uint256 _flowerId) external view returns(uint32 _type, uint32 _color) {
        //'cause we have type 0 and color 0, we need to differ it from a non-exist FlowerAuction
        if (_flowerId < flowers.length) {
            return (flowers[_flowerId].flowerType, flowers[_flowerId].flowerColor);
        } else {
            return (999,999);
        }
    }
}

//It is better to create a contract named Insurance like ERC20. Different kind of insurance just need to realize the predefined contents in Insurance.
contract MariageInsurance is FlowerCore {

	event Proposal(address customerOne, address customerTwo, uint256 index);
	event Change(uint256 index, uint32 status);
	//event Agree(uint256 index);
	//event Pass(uint256 index);
	//event Reject(uint256 index);
	//event Suspend(uint256 index);
	//event Complete(uint256 index);
	//event Cheat(uint256 index);
	
	uint256 public fee;
	function changeFee(uint256 newFee) onlyOwner whenNotPaused {
	    require((newFee >= 100000) && (newFee < 1 ether));
	    fee = newFee;
	}
	uint256 public profit;
	function changeProfit(uint256 newProfit) onlyOwner whenNotPaused {
	    require((newProfit>= 100) && (newProfit <= 250));
	    profit = newProfit;
	}
	
	struct Insurance {
		address customerOne;
		address customerTwo;
		uint256 duration;
		uint64 time;
		uint256 fundValue;
		uint32 status;
	}
	
	Insurance[] insurances;
	
	mapping(address => uint256) public customerToInsuranceIndex;
	mapping(uint256 => string) public insuranceIndexToCert;
	
	address public auditor;
	function changeAuditor(address newAuditor) onlyOwner whenNotPaused {
		if(newAuditor != address(0)) {
			auditor = newAuditor;
		}
	}
	modifier onlyAuditor() {
		require(msg.sender == auditor);
		_;
	}
	
	function MariageInsurance() public {
		auditor = msg.sender;
		fee = 10000;
		profit = 120;
		insurances.push(Insurance(address(0), address(0), 0, 0, 0, 0));
	}
	
	//Only returns its status. Just add more return values if needed.
	function getInsuranceByIndex(uint256 _index) external view returns(uint32) {
		Insurance storage _insurance = insurances[_index];
		return _insurance.status;
	}
	
	function getInsuranceIndex() external view returns(uint256) {
		return customerToInsuranceIndex[msg.sender];
	}
	
	
	function submitProposal(address _lover, uint256 _value, uint256 _duration) external payable whenNotPaused {
		require(customerToInsuranceIndex[msg.sender] == 0);
		require(customerToInsuranceIndex[_lover] == 0);
		require(msg.value >= _value + fee);
		uint256 excess = msg.value - _value - fee; 
		Insurance memory _insurance = Insurance({
			customerOne: msg.sender,
			customerTwo: _lover,
			duration: _duration,
			time: uint64(now),
			fundValue: _value,
			status: 0 
		});
		uint256 insuranceId = insurances.push(_insurance);
		require(insuranceId == uint256(uint32(insuranceId)));
		customerToInsuranceIndex[_lover] = insuranceId;
		customerToInsuranceIndex[msg.sender] = insuranceId;
		msg.sender.transfer(excess);
		Proposal(msg.sender, _lover, insuranceId);
	}
	
	function agreeWithProposal() external payable whenNotPaused {
		uint256 _index = customerToInsuranceIndex[msg.sender];
		require(_index > 0);
		Insurance storage _insurance = insurances[_index];
		require(_insurance.status == 0);
		require(_insurance.customerTwo == msg.sender);
		require(msg.value >= _insurance.fundValue);
		uint256 excess = msg.value - _insurance.fundValue;
		_insurance.status = 1;
		msg.sender.transfer(excess);
		Change(_index, 1);
		//Agree(_index);
	}
	
	function auditProposalWithYes(uint256 _index) external onlyAuditor whenNotPaused {
	    require(_index > 0);
		Insurance storage _insurance = insurances[_index];
		require(_insurance.status == 1);
		_insurance.status = 2;
		Change(_index, 2);
		//Pass(_index);
	}
	
	function auditProposalWithNo(uint256 _index) external onlyAuditor whenNotPaused {
	    require(_index > 0);
		Insurance storage _insurance = insurances[_index];
		require(_insurance.status == 1);
		_insurance.status == 8;
		customerToInsuranceIndex[_insurance.customerOne] = 0;
		customerToInsuranceIndex[_insurance.customerTwo] = 0;
		_insurance.customerOne.transfer(_insurance.fundValue);
		_insurance.customerTwo.transfer(_insurance.fundValue);
		Change(_index, 8);
		//Reject(_index);
	}
	
	function suspendInsurance() external whenNotPaused {
		uint256 _index = customerToInsuranceIndex[msg.sender];
		require(_index > 0);
		Insurance storage _insurance = insurances[_index];
		require(_insurance.status == 2);
		require(uint64(now) < _insurance.time + uint64(_insurance.duration * 86400));
		_insurance.status = 9;
		customerToInsuranceIndex[_insurance.customerOne] = 0;
		customerToInsuranceIndex[_insurance.customerTwo] = 0;
		msg.sender.transfer(_insurance.fundValue * 8 / 10);
		Change(_index, 9);
		//Suspend(_index);
	}
	
	function submitCert(string _source) external whenNotPaused {
		uint256 _index = customerToInsuranceIndex[msg.sender];
		require(_index > 0);
		Insurance storage _insurance = insurances[_index];
		require(_insurance.status == 2);
		require(uint64(now) > _insurance.time + uint64(_insurance.duration * 86400));
		insuranceIndexToCert[_index] = _source;
		_insurance.status = 3;
		Change(_index, 3);
	}
	
	function _computeFunds() internal view returns(uint256, uint256){
		uint256 count = insurances.length;
		uint256 activeCount = 0;
		uint256 limit = 0;
		for (uint i = 1; i < count; i++) {
			if (insurances[i].status < 7 ) {
				limit = limit + insurances[i].fundValue * 2;
				activeCount++;
			}
		}
		return (limit, activeCount);
	}
	
	function completeInsuranceWithYes(uint256 _index) external onlyAuditor whenNotPaused {
	    require(_index > 0);
		Insurance storage _insurance = insurances[_index];
		require(_insurance.status == 3);
		var (limit, activeCount) = _computeFunds();
		require(this.balance >= limit + activeCount * fee + _insurance.fundValue * 2 * (profit - 100) / 100);
		_insurance.status = 10;
		_insurance.customerOne.transfer(_insurance.fundValue * profit / 100);
		_insurance.customerTwo.transfer(_insurance.fundValue * profit / 100);
		Change(_index, 10);
		//Complete(_index);
	}
	
	function completeInsuranceWithNo(uint256 _index) external onlyAuditor whenNotPaused {
	    require(_index > 0);
		Insurance storage _insurance = insurances[_index];
		require(_insurance.status == 3);
		_insurance.status = 11;
		//remain its index. Couldn't submit a new proposal.
		//customerToInsuranceIndex[_insurance.customerOne] = 0;
		//customerToInsuranceIndex[_insurance.customerTwo] = 0;
		Change(_index, 11);
		//Cheat(_index);
	}
	
	//out of time
	/*
	function dropInsurance(uint256 _index) external onlyAuditor whenNotPaused {
	    require(_index > 0);
		Insurance storage _insurance = insurances[_index];
		require((_insurance.status == 2) && (now >= _insurance.time + uint64((_insurance.duration + 90) * 86400)));
		_insurance.status = 12;
		customerToInsuranceIndex[_insurance.customerOne] = 0;
		customerToInsuranceIndex[_insurance.customerTwo] = 0;
		Change(_index, 12);
	}
	*/
	
	
	function withdrawFunds(address _to) external onlyOwner {
	    require(_to != address(0));
		var (limit, activeCount) = _computeFunds();
		uint256 balance = this.balance;
		require(balance > limit + activeCount * fee);
		uint256 excess = balance - activeCount * fee;
		_to.transfer(excess);
	}
	
	//used to add funds and send back its ether if caller is not owner
	function() payable {
	    require((msg.sender == owner) || (msg.sender == address(saleAuction)));
	}
}

contract QuickLoveCore is MariageInsurance, Expressionwall {
    
    event ContractUpgrade(address newAddress);
    address public newContractAddress;
    
    function QuickLoveCore() public {
        paused = true;
    }
    
    function setNewAddress(address newAddress) external onlyOwner whenPaused {
        newContractAddress = newAddress;
        ContractUpgrade(newAddress);
    }
    
    function unpause() public onlyOwner whenPaused returns(bool) {
        require(address(saleAuction) != address(0));
        paused = false;
        Unpause();
        return true;
    }
}


