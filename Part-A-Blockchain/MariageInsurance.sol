pragma solidity ^0.4.11;
/*
//need cpmplete event
contract Expressionwall {
	event CreateExpress(address, string, uint256);
	mapping (uint256 => address) public expressionIndexToOwner;
	//mapping (address => uint256) public ownershipExpressionCount;
	mapping (address => uint256[]) public ownershipExpressionIdList;
	
	struct Expression {
		uint64 time;
		string data;
	}
	
	Expression[] expressiones;
	
	function createExpress(string _data) external returns(uint) {
		Expression _expression = Expression({
			time: uint64(now),
			data: _data
		});
		uint256 expressionId = expressiones.push(_expression) - 1;
		require(expressionId == uint256(uint32(expressionId)));
		expressionIndexToOwner[expressionId] = msg.sender;
		//ownershipExpressionCount[msg.sender] ++;
		ownershipExpressionIdList[msg.sender].push(expressionId);
		CreateExpress(msg.sender, _data, expressionId);
		return expressionId;
	}
	
	function getExpressionByIndex(uint256 _index) external returns(uint64 time, string data) {
		time = expressiones[_index].time;
		data = expressiones[_index].data;
	}
	
	function getMyExpressionByIndex(uint256 _index) external returns(uint64 time, string data) {
		time = expressiones[ownershipExpressionIdList[msg.sender][_index]].time;
		data = expressiones[ownershipExpressionIdList[msg.sender][_index]].data;
	}
	//struct can not be used in external function return
	function getAllExpression() external returns(uint256 count, uint64[] times, string[] expressiones) {
		count = expressiones.length;
		for (uint i = 0; i< count; i++) {
			times.push(expressiones[i].time);
			expressions.push(expressiones[i].data);
		}
	}
}
*/

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


contract MariageInsurance is Pausable {

	event Proposal(address customerOne, address customerTwo, uint256 index);
	event Agree(uint256 index);
	event Pass(uint256 index);
	event Reject(uint256 index);
	event Suspend(uint256 index);
	event Complete(uint256 index);
	event Cheat(uint256 index);
	
	uint256 public fee;
	uint256 public profit;
	
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
	/*
	function MaraigeInsurance(address _auditor, uint _fee) public {
		require(_auditor != address(0));
		auditor = _auditor;
		fee = _fee;
	}*/
	
	function MariageInsurance() public {
		auditor = msg.sender;
		fee = 10000;
		profit = 120;
	}
	
	function getInsuranceByIndex(uint256 _index) external whenNotPaused returns(uint32) {
		Insurance storage _insurance = insurances[_index - 1];
		return _insurance.status;
	}
	
	function getInsuranceIndex() external whenNotPaused returns(uint256) {
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
		})
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
		Insurance storage _insurance = insurances[_index - 1];
		require(_insurance.status == 0);
		require(msg.value >= _insurance.fundValue);
		uint256 excess = msg.value - _insurance.fundValue;
		_insurance.status = 1;
		msg.sender.transfer(excess);
		Agree(_index);
	}
	
	function auditProposalWithYes(uint256 _index) external onlyAuditor whenNotPaused {
		Insurance storage _insurance = insurances[_index - 1];
		require(_insurance.status == 1);
		_insurance.status = 2;
		Pass(_index);
	}
	
	function auditProposalWithNo(uint256 _index) external onlyAuditor whenNotPaused {
		Insurance storage _insurance = insurances[_index - 1];
		require(_insurance.status == 1);
		_insurance.status == 8;
		customerToInsuranceIndex[_insurance.customerOne] = 0;
		customerToInsuranceIndex[_insurance.customerTwo] = 0;
		_insurance.customerOne.transfer(_insurance.fundValue);
		_insurance.customerTwo.transfer(_insurance.fundValue);
		Reject(_index);
	}
	
	function suspendInsurance() external whenNotPaused {
		uint256 _index = customerToInsuranceIndex[msg.sender];
		require(_index > 0);
		Insurance storage _insurance = insurances[_index - 1];
		require(_insurance.status == 2);
		require(uint64(now) < _insurance.time + uint64(_insurance.duration));
		_insurance.status = 9;
		customerToInsuranceIndex[_insurance.customerOne] = 0;
		customerToInsuranceIndex[_insurance.customerTwo] = 0;
		msg.sender.transfer(_insurance.fundValue * 8 / 10);
		Suspend(_index)
	}
	
	function submitCert(string _source) external whenNotPaused {
		uint256 _index = customerToInsuranceIndex[msg.sender];
		require(_index > 0);
		Insurance storage _insurance = insurances[_index - 1];
		require(_insurance.status == 2);
		require(uint64(now) > _insurance.time + uint64(_insurance.duration));
		insuranceIndexToCert[_index] = _source;
		_instance.status = 3;
	}
	
	function completeInsuranceWithYes(uint256 _index) external onlyAuditor whenNotPaused {
		Insurance storage _insurance = insurances[_index - 1];
		require(_insurance.status == 3);
		_insurance.status = 4;
		_instance.customerOne.transfer(_insurance.fundValue * profit / 100);
		_instance.customerTwo.transfer(_insurance.fundValue * profit / 100);
		Complete(_index);
	}
	
	function completeInsuranceWithNo(uint256 _index) external onlyAuditor whenNotPaused {
		Insurance storage _insurance = insurances[_index - 1];
		require(_insurance.status == 3);
		_insurance.status = 10;
		customerToInsuranceIndex[_insurance.customerOne] = 0;
		customerToInsuranceIndex[_insurance.customerTwo] = 0;
		Cheat(_index);
	}
	
	function _computeFunds() internal returns(uint256){
		uint256 count = insurances.length;
		uint256 limit = 0;
		for (uint i =0; i < count; i ++) {
			if (insurances[i]).status < 7 ) {
				limit = limit + insurances[i].fundValue * 2;
			}
		}
		return limit;
	}
	
	function withdrawFunds(address _to) external onlyOwner {
		uint256 limit = _computeFunds();
		uint256 balance = this.balance;
		require(balance > limit + insurances.length * fee);
		uint256 excess = balance - insurances.length * fee;
		_to.transfer(excess);
	}
}

contract FlowerAccessControl {
	address public ceoAddress;
	
	modifier onlyCEO
}