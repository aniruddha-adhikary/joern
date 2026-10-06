package loan;

public class Report {
  public boolean approved;
  public double insuranceRate;
  public boolean insuranceRequired;
  public double monthlyRepayment;
  public boolean validData;
  public double yearlyInterestRate;
  public double yearlyRepayment;

  public void addMessage(String message) {}
  public void approveLoan(String message) {}
  public void rejectData(String message) {}
  public void rejectLoan(String message) {}
  public String toString() { return ""; }
}
