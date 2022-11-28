// Generated by rstantools.  Do not edit by hand.

/*
    BayesDeming is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    BayesDeming is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BayesDeming.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.21.0
#include <stan/model/model_header.hpp>
namespace model_deming_power_namespace {
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
static int current_statement_begin__;
stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_deming_power");
    reader.add_event(88, 86, "end", "model_deming_power");
    return reader;
}
#include <stan_meta_header.hpp>
class model_deming_power
  : public stan::model::model_base_crtp<model_deming_power> {
private:
        int Nx;
        int Ny;
        int K;
        std::vector<int> Jx;
        std::vector<int> Jy;
        vector_d X;
        vector_d Y;
        double sigma_x_max;
        double sigma_y_max;
        double alpha_var;
        double beta_var;
        double alpha_mean;
        double beta_mean;
        int Nz;
        vector_d z;
        double power;
        vector_d z_demeaned;
public:
    model_deming_power(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, 0, pstream__);
    }
    model_deming_power(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, random_seed__, pstream__);
    }
    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning
        current_statement_begin__ = -1;
        static const char* function__ = "model_deming_power_namespace::model_deming_power";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        try {
            // initialize data block variables from context__
            current_statement_begin__ = 3;
            context__.validate_dims("data initialization", "Nx", "int", context__.to_vec());
            Nx = int(0);
            vals_i__ = context__.vals_i("Nx");
            pos__ = 0;
            Nx = vals_i__[pos__++];
            check_greater_or_equal(function__, "Nx", Nx, 0);
            current_statement_begin__ = 4;
            context__.validate_dims("data initialization", "Ny", "int", context__.to_vec());
            Ny = int(0);
            vals_i__ = context__.vals_i("Ny");
            pos__ = 0;
            Ny = vals_i__[pos__++];
            check_greater_or_equal(function__, "Ny", Ny, 0);
            current_statement_begin__ = 7;
            context__.validate_dims("data initialization", "K", "int", context__.to_vec());
            K = int(0);
            vals_i__ = context__.vals_i("K");
            pos__ = 0;
            K = vals_i__[pos__++];
            check_greater_or_equal(function__, "K", K, 0);
            current_statement_begin__ = 10;
            validate_non_negative_index("Jx", "K", K);
            context__.validate_dims("data initialization", "Jx", "int", context__.to_vec(K));
            Jx = std::vector<int>(K, int(0));
            vals_i__ = context__.vals_i("Jx");
            pos__ = 0;
            size_t Jx_k_0_max__ = K;
            for (size_t k_0__ = 0; k_0__ < Jx_k_0_max__; ++k_0__) {
                Jx[k_0__] = vals_i__[pos__++];
            }
            current_statement_begin__ = 11;
            validate_non_negative_index("Jy", "K", K);
            context__.validate_dims("data initialization", "Jy", "int", context__.to_vec(K));
            Jy = std::vector<int>(K, int(0));
            vals_i__ = context__.vals_i("Jy");
            pos__ = 0;
            size_t Jy_k_0_max__ = K;
            for (size_t k_0__ = 0; k_0__ < Jy_k_0_max__; ++k_0__) {
                Jy[k_0__] = vals_i__[pos__++];
            }
            current_statement_begin__ = 14;
            validate_non_negative_index("X", "Nx", Nx);
            context__.validate_dims("data initialization", "X", "vector_d", context__.to_vec(Nx));
            X = Eigen::Matrix<double, Eigen::Dynamic, 1>(Nx);
            vals_r__ = context__.vals_r("X");
            pos__ = 0;
            size_t X_j_1_max__ = Nx;
            for (size_t j_1__ = 0; j_1__ < X_j_1_max__; ++j_1__) {
                X(j_1__) = vals_r__[pos__++];
            }
            current_statement_begin__ = 15;
            validate_non_negative_index("Y", "Ny", Ny);
            context__.validate_dims("data initialization", "Y", "vector_d", context__.to_vec(Ny));
            Y = Eigen::Matrix<double, Eigen::Dynamic, 1>(Ny);
            vals_r__ = context__.vals_r("Y");
            pos__ = 0;
            size_t Y_j_1_max__ = Ny;
            for (size_t j_1__ = 0; j_1__ < Y_j_1_max__; ++j_1__) {
                Y(j_1__) = vals_r__[pos__++];
            }
            current_statement_begin__ = 18;
            context__.validate_dims("data initialization", "sigma_x_max", "double", context__.to_vec());
            sigma_x_max = double(0);
            vals_r__ = context__.vals_r("sigma_x_max");
            pos__ = 0;
            sigma_x_max = vals_r__[pos__++];
            check_greater_or_equal(function__, "sigma_x_max", sigma_x_max, 0);
            current_statement_begin__ = 19;
            context__.validate_dims("data initialization", "sigma_y_max", "double", context__.to_vec());
            sigma_y_max = double(0);
            vals_r__ = context__.vals_r("sigma_y_max");
            pos__ = 0;
            sigma_y_max = vals_r__[pos__++];
            check_greater_or_equal(function__, "sigma_y_max", sigma_y_max, 0);
            current_statement_begin__ = 20;
            context__.validate_dims("data initialization", "alpha_var", "double", context__.to_vec());
            alpha_var = double(0);
            vals_r__ = context__.vals_r("alpha_var");
            pos__ = 0;
            alpha_var = vals_r__[pos__++];
            check_greater_or_equal(function__, "alpha_var", alpha_var, 0);
            current_statement_begin__ = 21;
            context__.validate_dims("data initialization", "beta_var", "double", context__.to_vec());
            beta_var = double(0);
            vals_r__ = context__.vals_r("beta_var");
            pos__ = 0;
            beta_var = vals_r__[pos__++];
            check_greater_or_equal(function__, "beta_var", beta_var, 0);
            current_statement_begin__ = 22;
            context__.validate_dims("data initialization", "alpha_mean", "double", context__.to_vec());
            alpha_mean = double(0);
            vals_r__ = context__.vals_r("alpha_mean");
            pos__ = 0;
            alpha_mean = vals_r__[pos__++];
            current_statement_begin__ = 23;
            context__.validate_dims("data initialization", "beta_mean", "double", context__.to_vec());
            beta_mean = double(0);
            vals_r__ = context__.vals_r("beta_mean");
            pos__ = 0;
            beta_mean = vals_r__[pos__++];
            current_statement_begin__ = 26;
            context__.validate_dims("data initialization", "Nz", "int", context__.to_vec());
            Nz = int(0);
            vals_i__ = context__.vals_i("Nz");
            pos__ = 0;
            Nz = vals_i__[pos__++];
            check_greater_or_equal(function__, "Nz", Nz, 0);
            current_statement_begin__ = 27;
            validate_non_negative_index("z", "Nz", Nz);
            context__.validate_dims("data initialization", "z", "vector_d", context__.to_vec(Nz));
            z = Eigen::Matrix<double, Eigen::Dynamic, 1>(Nz);
            vals_r__ = context__.vals_r("z");
            pos__ = 0;
            size_t z_j_1_max__ = Nz;
            for (size_t j_1__ = 0; j_1__ < z_j_1_max__; ++j_1__) {
                z(j_1__) = vals_r__[pos__++];
            }
            current_statement_begin__ = 30;
            context__.validate_dims("data initialization", "power", "double", context__.to_vec());
            power = double(0);
            vals_r__ = context__.vals_r("power");
            pos__ = 0;
            power = vals_r__[pos__++];
            // initialize transformed data variables
            current_statement_begin__ = 34;
            validate_non_negative_index("z_demeaned", "Nz", Nz);
            z_demeaned = Eigen::Matrix<double, Eigen::Dynamic, 1>(Nz);
            stan::math::fill(z_demeaned, DUMMY_VAR__);
            // execute transformed data statements
            current_statement_begin__ = 35;
            stan::math::assign(z_demeaned, subtract(z, mean(z)));
            // validate transformed data
            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 39;
            num_params_r__ += 1;
            current_statement_begin__ = 40;
            num_params_r__ += 1;
            current_statement_begin__ = 41;
            validate_non_negative_index("theta", "K", K);
            num_params_r__ += K;
            current_statement_begin__ = 43;
            num_params_r__ += 1;
            current_statement_begin__ = 44;
            num_params_r__ += 1;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    ~model_deming_power() { }
    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;
        current_statement_begin__ = 39;
        if (!(context__.contains_r("alpha")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable alpha missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("alpha");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "alpha", "double", context__.to_vec());
        double alpha(0);
        alpha = vals_r__[pos__++];
        try {
            writer__.scalar_unconstrain(alpha);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable alpha: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 40;
        if (!(context__.contains_r("beta")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable beta missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("beta");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "beta", "double", context__.to_vec());
        double beta(0);
        beta = vals_r__[pos__++];
        try {
            writer__.scalar_unconstrain(beta);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable beta: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 41;
        if (!(context__.contains_r("theta")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable theta missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("theta");
        pos__ = 0U;
        validate_non_negative_index("theta", "K", K);
        context__.validate_dims("parameter initialization", "theta", "vector_d", context__.to_vec(K));
        Eigen::Matrix<double, Eigen::Dynamic, 1> theta(K);
        size_t theta_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < theta_j_1_max__; ++j_1__) {
            theta(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(theta);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable theta: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 43;
        if (!(context__.contains_r("sigma_x")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable sigma_x missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("sigma_x");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "sigma_x", "double", context__.to_vec());
        double sigma_x(0);
        sigma_x = vals_r__[pos__++];
        try {
            writer__.scalar_lub_unconstrain(0, sigma_x_max, sigma_x);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable sigma_x: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 44;
        if (!(context__.contains_r("sigma_y")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable sigma_y missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("sigma_y");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "sigma_y", "double", context__.to_vec());
        double sigma_y(0);
        sigma_y = vals_r__[pos__++];
        try {
            writer__.scalar_lub_unconstrain(0, sigma_y_max, sigma_y);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable sigma_y: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }
    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }
    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {
        typedef T__ local_scalar_t__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning
        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
            // model parameters
            current_statement_begin__ = 39;
            local_scalar_t__ alpha;
            (void) alpha;  // dummy to suppress unused var warning
            if (jacobian__)
                alpha = in__.scalar_constrain(lp__);
            else
                alpha = in__.scalar_constrain();
            current_statement_begin__ = 40;
            local_scalar_t__ beta;
            (void) beta;  // dummy to suppress unused var warning
            if (jacobian__)
                beta = in__.scalar_constrain(lp__);
            else
                beta = in__.scalar_constrain();
            current_statement_begin__ = 41;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> theta;
            (void) theta;  // dummy to suppress unused var warning
            if (jacobian__)
                theta = in__.vector_constrain(K, lp__);
            else
                theta = in__.vector_constrain(K);
            current_statement_begin__ = 43;
            local_scalar_t__ sigma_x;
            (void) sigma_x;  // dummy to suppress unused var warning
            if (jacobian__)
                sigma_x = in__.scalar_lub_constrain(0, sigma_x_max, lp__);
            else
                sigma_x = in__.scalar_lub_constrain(0, sigma_x_max);
            current_statement_begin__ = 44;
            local_scalar_t__ sigma_y;
            (void) sigma_y;  // dummy to suppress unused var warning
            if (jacobian__)
                sigma_y = in__.scalar_lub_constrain(0, sigma_y_max, lp__);
            else
                sigma_y = in__.scalar_lub_constrain(0, sigma_y_max);
            // transformed parameters
            current_statement_begin__ = 48;
            validate_non_negative_index("nu", "K", K);
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> nu(K);
            stan::math::initialize(nu, DUMMY_VAR__);
            stan::math::fill(nu, DUMMY_VAR__);
            // transformed parameters block statements
            current_statement_begin__ = 49;
            stan::math::assign(nu, add(alpha, multiply(beta, theta)));
            // validate transformed parameters
            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning
            current_statement_begin__ = 48;
            size_t nu_j_1_max__ = K;
            for (size_t j_1__ = 0; j_1__ < nu_j_1_max__; ++j_1__) {
                if (stan::math::is_uninitialized(nu(j_1__))) {
                    std::stringstream msg__;
                    msg__ << "Undefined transformed parameter: nu" << "(" << j_1__ << ")";
                    stan::lang::rethrow_located(std::runtime_error(std::string("Error initializing variable nu: ") + msg__.str()), current_statement_begin__, prog_reader__());
                }
            }
            // model body
            {
            current_statement_begin__ = 54;
            int pos(0);
            (void) pos;  // dummy to suppress unused var warning
            stan::math::fill(pos, std::numeric_limits<int>::min());
            current_statement_begin__ = 57;
            lp_accum__.add(normal_log<propto__>(alpha, alpha_mean, alpha_var));
            current_statement_begin__ = 58;
            lp_accum__.add(normal_log<propto__>(beta, beta_mean, beta_var));
            current_statement_begin__ = 61;
            stan::math::assign(pos, 1);
            current_statement_begin__ = 62;
            for (int k = 1; k <= K; ++k) {
                current_statement_begin__ = 63;
                lp_accum__.add(normal_log<propto__>(segment(X, pos, get_base1(Jx, k, "Jx", 1)), get_base1(theta, k, "theta", 1), sigma_x));
                current_statement_begin__ = 64;
                stan::math::assign(pos, (pos + get_base1(Jx, k, "Jx", 1)));
            }
            current_statement_begin__ = 68;
            stan::math::assign(pos, 1);
            current_statement_begin__ = 69;
            for (int k = 1; k <= K; ++k) {
                current_statement_begin__ = 70;
                lp_accum__.add(normal_log<propto__>(segment(Y, pos, get_base1(Jy, k, "Jy", 1)), get_base1(nu, k, "nu", 1), sigma_y));
                current_statement_begin__ = 71;
                stan::math::assign(pos, (pos + get_base1(Jy, k, "Jy", 1)));
            }
            current_statement_begin__ = 76;
            lp_accum__.add((power * normal_log(z_demeaned, 0, sigma_x)));
            }
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
        lp_accum__.add(lp__);
        return lp_accum__.sum();
    } // log_prob()
    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }
    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("alpha");
        names__.push_back("beta");
        names__.push_back("theta");
        names__.push_back("sigma_x");
        names__.push_back("sigma_y");
        names__.push_back("nu");
    }
    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(K);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(K);
        dimss__.push_back(dims__);
    }
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;
        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_deming_power_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        double alpha = in__.scalar_constrain();
        vars__.push_back(alpha);
        double beta = in__.scalar_constrain();
        vars__.push_back(beta);
        Eigen::Matrix<double, Eigen::Dynamic, 1> theta = in__.vector_constrain(K);
        size_t theta_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < theta_j_1_max__; ++j_1__) {
            vars__.push_back(theta(j_1__));
        }
        double sigma_x = in__.scalar_lub_constrain(0, sigma_x_max);
        vars__.push_back(sigma_x);
        double sigma_y = in__.scalar_lub_constrain(0, sigma_y_max);
        vars__.push_back(sigma_y);
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        if (!include_tparams__ && !include_gqs__) return;
        try {
            // declare and define transformed parameters
            current_statement_begin__ = 48;
            validate_non_negative_index("nu", "K", K);
            Eigen::Matrix<double, Eigen::Dynamic, 1> nu(K);
            stan::math::initialize(nu, DUMMY_VAR__);
            stan::math::fill(nu, DUMMY_VAR__);
            // do transformed parameters statements
            current_statement_begin__ = 49;
            stan::math::assign(nu, add(alpha, multiply(beta, theta)));
            if (!include_gqs__ && !include_tparams__) return;
            // validate transformed parameters
            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning
            // write transformed parameters
            if (include_tparams__) {
                size_t nu_j_1_max__ = K;
                for (size_t j_1__ = 0; j_1__ < nu_j_1_max__; ++j_1__) {
                    vars__.push_back(nu(j_1__));
                }
            }
            if (!include_gqs__) return;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }
    std::string model_name() const {
        return "model_deming_power";
    }
    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "alpha";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "beta";
        param_names__.push_back(param_name_stream__.str());
        size_t theta_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < theta_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "theta" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_x";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_y";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
            size_t nu_j_1_max__ = K;
            for (size_t j_1__ = 0; j_1__ < nu_j_1_max__; ++j_1__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "nu" << '.' << j_1__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        if (!include_gqs__) return;
    }
    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "alpha";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "beta";
        param_names__.push_back(param_name_stream__.str());
        size_t theta_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < theta_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "theta" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_x";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_y";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
            size_t nu_j_1_max__ = K;
            for (size_t j_1__ = 0; j_1__ < nu_j_1_max__; ++j_1__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "nu" << '.' << j_1__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        if (!include_gqs__) return;
    }
}; // model
}  // namespace
typedef model_deming_power_namespace::model_deming_power stan_model;
#ifndef USING_R
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
#endif
#endif
