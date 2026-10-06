package loan;

public class Borrower {
  public int age;
  public int creditScore;
  public String lastName;
  public Ssn ssn;
  public double yearlyIncome;
  public String zipCode;

  public int getBankruptcyAge() { return 0; }
  public boolean hasLatestBankrupcy() { return false; }
  public boolean checkAge() { return true; }
  public boolean checkName() { return true; }
  public boolean checkSSNareanumber() { return true; }
  public boolean checkSSNdigits() { return true; }
  public boolean checkZipcode() { return true; }
  public String toString() { return ""; }
}
